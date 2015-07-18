unit untMain;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ComCtrls, ExtDlgs, ExtCtrls, dglOpenGL, glWrappers;

type
  { TPanel }
  TRenderMethod = procedure of object;

  TPanel = class(ExtCtrls.TPanel)
  private
    FRender: TRenderMethod;
  protected
    procedure Paint; override;
  public
    procedure EraseBackground(DC: HDC); override;
    procedure SetRenderMethod(proc: TRenderMethod);
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    btnOpen: TButton;
    cbMod16: TCheckBox;
    cbRenderOnIdle: TCheckBox;
    lblDistance: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    pnlRender: TPanel;
    rbR8: TRadioButton;
    rbR16: TRadioButton;
    rbR32F: TRadioButton;
    tbDistance: TTrackBar;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure btnOpenClick(Sender: TObject);
    procedure cbMod16Change(Sender: TObject);
    procedure rbR8Change(Sender: TObject);
    procedure tbDistanceChange(Sender: TObject);
  private
    FDC: HDC;
    FRC: HGLRC;
    FVBO: TGLvbo;
    FSrcTex: TGLtexrect;

    FHProg: TGLprog;
    FVProg: TGLprog;
    FVfield: TGLtexrect;
    FVPass: TGlfbo;

    FFPSLastTime: Integer;
    FFPSCount: Integer;
  protected
    function GetVFieldFormat: GLenum;
    procedure ReloadShaders;
    procedure RebuildFBO;
    procedure LoadImage(filename: string);
    procedure InitGLResources;
    procedure FreeGLResources;

    procedure ChildHandlesCreated; override;
    procedure FinalizeWnd; override;

    procedure RenderScene;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TPanel }

procedure TPanel.Paint;
begin
  if assigned(FRender) then FRender();
end;

procedure TPanel.EraseBackground(DC: HDC);
begin
  //nothing do here
end;

procedure TPanel.SetRenderMethod(proc: TRenderMethod);
begin
  FRender := proc;
end;

{ TfrmMain }

procedure TfrmMain.rbR8Change(Sender: TObject);
begin
  if TRadioButton(Sender).Checked then
  begin
    wglMakeCurrent(FDC, FRC);
    RebuildFBO;
    wglMakeCurrent(0, 0);
    pnlRender.Invalidate;
  end;
end;

procedure TfrmMain.tbDistanceChange(Sender: TObject);
begin
  lblDistance.Caption:='Field distance: '+IntToStr(tbDistance.Position);
  pnlRender.Invalidate;
end;

function TfrmMain.GetVFieldFormat: GLenum;
begin
  Result := GL_R8; //default value
  if rbR8.Checked then Exit(GL_R8);
  if rbR16.Checked then Exit(GL_R16);
  if rbR32F.Checked then Exit(GL_R32F);
  pnlRender.Invalidate;
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    wglMakeCurrent(FDC, FRC);
    LoadImage(OpenPictureDialog.FileName);
    wglMakeCurrent(0, 0);
    pnlRender.Invalidate;
  end;
end;

procedure TfrmMain.cbMod16Change(Sender: TObject);
begin
  pnlRender.Invalidate;
end;

procedure TfrmMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean
  );
begin
  if cbRenderOnIdle.Checked then
  begin
    pnlRender.Invalidate;
    Done := False;
  end;
end;

procedure TfrmMain.ReloadShaders;
begin
  FreeAndNil(FHProg);
  FreeAndNil(FVProg);
  FHProg := TGLprog.Create('shaders\hfield.vert', 'shaders\hfield.frag');
  FVProg := TGLprog.Create('shaders\vfield.vert', 'shaders\vfield.frag');
end;

procedure TfrmMain.RebuildFBO;
begin
  FreeAndNil(FVfield);
  FreeAndNil(FVPass);
  FVfield := TGLtexrect.Create(Trunc(FSrcTex.TexSize.x), Trunc(FSrcTex.TexSize.y), GetVFieldFormat, GL_RGBA, GL_UNSIGNED_BYTE, Nil);
  FVPass := TGLfbo.Create(FVfield);
end;

procedure TfrmMain.LoadImage(filename: string);
begin
  FreeAndNil(FSrcTex);
  FSrcTex := TGLtexrect.Create(filename);
  pnlRender.ClientWidth := Trunc(FSrcTex.TexSize.x);
  pnlRender.ClientHeight := Trunc(FSrcTex.TexSize.y);
  RebuildFBO;
  ClientWidth := pnlRender.Left + pnlRender.Width;
  ClientHeight := pnlRender.Top + pnlRender.Height;
end;

procedure TfrmMain.InitGLResources;
begin
  FVBO   := TGLvbo.Create(@ScreenAlignedQuad[0], 4);
  ReloadShaders;
  LoadImage('test.png');
end;

procedure TfrmMain.FreeGLResources;
begin
  FreeAndNil(FHProg);
  FreeAndNil(FVProg);
  FreeAndNil(FVBO);
  FreeAndNil(FSrcTex);
  FreeAndNil(FVfield);
  FreeAndNil(FVPass);
end;

procedure TfrmMain.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  FDC := GetDC(pnlRender.Handle);
  FRC := CreateRenderingContextVersion(FDC, [opDoubleBuffered], 3, 0, False, 32, 0, 0, 0, 0, 0);
  ActivateRenderingContext(FDC, FRC);
  wglSwapIntervalEXT(0);
  InitGLResources;
  DeactivateRenderingContext;
  pnlRender.SetRenderMethod(@RenderScene);
end;

procedure TfrmMain.FinalizeWnd;
begin
  wglMakeCurrent(FDC, FRC);
  FreeGLResources;

  wglDeleteContext(FRC);
  ReleaseDC(pnlRender.Handle, FDC);
  pnlRender.SetRenderMethod(Nil);
  inherited FinalizeWnd;
end;

procedure TfrmMain.RenderScene;
begin
  if FDC = 0 then Exit;
  if FRC = 0 then Exit;
  if wglMakeCurrent(FDC, FRC) then
  try
    glEnable(GL_TEXTURE_RECTANGLE);
    glViewport(0, 0, pnlRender.ClientWidth, pnlRender.ClientHeight);

    FVPass.Select;
      FVProg.Select;
      FVBO.Select;
      FSrcTex.Select(0);
      FVProg.SetAttr('aPos', 2, GL_FLOAT, 8, 0);
      FVProg.SetUniform('uTex1', 0);
      FVProg.SetUniform('uTexSize', FSrcTex.TexSize);
      FVProg.SetUniform('uDistance', tbDistance.Position*1.0);
      FVBO.Draw(GL_TRIANGLE_STRIP, 0, 4);
    TGLfbo.Deselect;

    FHProg.Select;
    FVBO.Select;
    FVfield.Select(0);
    FHProg.SetAttr('aPos', 2, GL_FLOAT, 8, 0);
    FHProg.SetUniform('uTex1', 0);
    FHProg.SetUniform('uTexSize', FVfield.TexSize);
    FHProg.SetUniform('uDistance', tbDistance.Position*1.0);
    if cbMod16.Checked then
      FHProg.SetUniform('uMod16', 1.0)
    else
      FHProg.SetUniform('uMod16', 0.0);
    FVBO.Draw(GL_TRIANGLE_STRIP, 0, 4);

    SwapBuffers(FDC);

    Inc(FFPSCount);
    if GetTickCount - FFPSLastTime > 1000 then
    begin
      FFPSLastTime := GetTickCount;
      Caption := 'FPS: '+IntToStr(FFPSCount);
      FFPSCount := 0;
    end;
  finally
    wglMakeCurrent(0,0);
  end;
end;

initialization
InitOpenGL;

finalization

end.

