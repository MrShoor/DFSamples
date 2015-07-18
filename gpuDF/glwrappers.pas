unit glWrappers;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, dglOpenGL;

type
  TVec2 = record
    x, y: Single;
  end;
  PVec2 = ^TVec2;

const
  ScreenAlignedQuad: array [0..3] of TVec2 = ((x:-1;y:-1), (x:-1;y:1), (x:1;y:-1), (x:1;y:1));

type

  { TGLprog }

  TGLprog = class
  private
    FProg   : GLHandle;
    FVShader: GLHandle;
    FFShader: GLHandle;
  public
    procedure Select;
    procedure SetAttr(Const AttrName: AnsiString; CompCount: Integer; CompType: GLenum; Stride, Offset: Integer);
    procedure SetUniform(Const UniformName: AnsiString; Const Value: Integer); overload;
    procedure SetUniform(Const UniformName: AnsiString; Const Value: Single); overload;
    procedure SetUniform(Const UniformName: AnsiString; Const Vec: TVec2); overload;
    constructor Create(VertexShader, FragmentShader: string);
    destructor Destroy; override;
  end;

  { TGLvbo }

  TGLvbo = class
  private
    FVert: GLuint;
  public
    procedure Select;
    procedure Draw(PrimType: GLuint; FromVertex, CountVertex: Integer);
    constructor Create(vertices: PVec2; verticesCount: Integer);
    destructor Destroy; override;
  end;

  { TGLtexrect }

  TGLtexrect = class
  private
    FTex: GLuint;
    FTexSize: TVec2;
  public
    property TexSize: TVec2 read FTexSize;
    procedure Select(Slot: Integer);
    constructor Create(filename: string); overload;
    constructor Create(Width, Height: Integer; InternalFormat, DataFormat, ComponentFormat: GLenum; Data: Pointer); overload;
    destructor Destroy; override;
  end;

  { TGLfbo }

  TGLfbo = class
  private
    FFBO: GLuint;
  public
    procedure Select;
    class procedure Deselect;
    constructor Create(color: TGltexrect);
    destructor Destroy; override;
  end;

implementation

uses Graphics, GraphType;

{ TGLfbo }

procedure TGLfbo.Select;
begin
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, FFBO);
end;

class procedure TGLfbo.Deselect;
begin
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
end;

constructor TGLfbo.Create(color: TGltexrect);
begin
  glGenFramebuffers(1, @FFBO);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, FFBO);
  glFramebufferTexture(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, color.FTex, 0);
  glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
end;

destructor TGLfbo.Destroy;
begin
  inherited Destroy;
end;

{ TGLtexrect }

procedure TGLtexrect.Select(Slot: Integer);
begin
  glActiveTexture(GL_TEXTURE0 + Slot);
  glBindTexture(GL_TEXTURE_RECTANGLE, FTex);
end;

constructor TGLtexrect.Create(filename: string);
var bmp: TBitmap;
    pic: TPicture;
    raw: TRawImage;
begin
  bmp := nil;
  pic := nil;
  try
    bmp := TBitmap.Create;
    pic := TPicture.Create;
    pic.LoadFromFile(filename);
    bmp.PixelFormat := pf32bit;
    bmp.Width := pic.Width;
    bmp.Height := pic.Height;
    bmp.Canvas.Draw(0, 0, pic.Graphic);
    raw := bmp.RawImage;
    Create(bmp.Width, bmp.Height, GL_RGBA, GL_RGBA, GL_UNSIGNED_BYTE, raw.Data);
  finally
    pic.Free;
    bmp.Free;
  end;
end;

constructor TGLtexrect.Create(Width, Height: Integer; InternalFormat,
  DataFormat, ComponentFormat: GLenum; Data: Pointer);
begin
  glGenTextures(1, @FTex);
  glBindTexture(GL_TEXTURE_RECTANGLE, FTex);
  glTexImage2D(GL_TEXTURE_RECTANGLE, 0, InternalFormat, Width, Height, 0, DataFormat, ComponentFormat, Data);
  FTexSize.x := Width;
  FTexSize.y := Height;
end;

destructor TGLtexrect.Destroy;
begin
  if FTex <> 0 then
     glDeleteTextures(1, @FTex);
  FTex := 0;
  inherited Destroy;
end;

{ TGLvbo }

procedure TGLvbo.Select;
begin
  glBindBuffer(GL_ARRAY_BUFFER, FVert);
end;

procedure TGLvbo.Draw(PrimType: GLuint; FromVertex, CountVertex: Integer);
begin
  glDrawArrays(PrimType, FromVertex, CountVertex);
end;

constructor TGLvbo.Create(vertices: PVec2; verticesCount: Integer);
begin
  glGenBuffers(1, @FVert);
  glBindBuffer(GL_ARRAY_BUFFER, FVert);
  glBufferData(GL_ARRAY_BUFFER, verticesCount*SizeOf(TVec2), vertices, GL_STATIC_DRAW);
end;

destructor TGLvbo.Destroy;
begin
  glDeleteBuffers(1, @FVert);
  inherited Destroy;
end;

{ TGLprog }

procedure TGLprog.Select;
begin
  glUseProgram(FProg);
end;

procedure TGLprog.SetAttr(const AttrName: AnsiString; CompCount: Integer;
  CompType: GLenum; Stride, Offset: Integer);
var AttrPos: GLint;
begin
  AttrPos := glGetAttribLocation(FProg, PGLChar(AttrName));
  glEnableVertexAttribArray(AttrPos);
  glVertexAttribPointer(AttrPos, CompCount, CompType, False, Stride, Pointer(Offset));
end;

procedure TGLprog.SetUniform(const UniformName: AnsiString; const Value: Integer
  );
var location: Integer;
begin
  location := glGetUniformLocation(FProg, PGLChar(UniformName));
  glUniform1i(location, Value);
end;

procedure TGLprog.SetUniform(const UniformName: AnsiString; const Value: Single
  );
var location: Integer;
begin
  location := glGetUniformLocation(FProg, PGLChar(UniformName));
  glUniform1f(location, Value);
end;

procedure TGLprog.SetUniform(const UniformName: AnsiString; const Vec: TVec2);
var location: Integer;
begin
  location := glGetUniformLocation(FProg, PGLChar(UniformName));
  glUniform2fv(location, 1, @Vec);
end;

constructor TGLprog.Create(VertexShader, FragmentShader: string);
  function LoadShader(target: GLenum; const data: AnsiString): GLHandle;
  var n: Integer;
  begin
    Result := glCreateShader(target);
    n := Length(data);
    glShaderSource(Result, 1, @data, @n);
    glCompileShader(Result);
  end;
var sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(VertexShader);
    FVShader:=LoadShader(GL_VERTEX_SHADER, Utf8ToAnsi(sl.Text));
    sl.LoadFromFile(FragmentShader);
    FFShader:=LoadShader(GL_FRAGMENT_SHADER, Utf8ToAnsi(sl.Text));
  finally
    FreeAndNil(sl);
  end;
  FProg:=glCreateProgram();
  glAttachShader(FProg, FVShader);
  glAttachShader(FProg, FFShader);
  glLinkProgram(FProg);
end;

destructor TGLprog.Destroy;
begin
  if FProg <> 0 then
  begin
    if (FVShader <> 0) then
    begin
      glDetachShader(FProg, FVShader);
      glDeleteShader(FVShader);
    end;
    if (FFShader <> 0) then
    begin
      glDetachShader(FProg, FFShader);
      glDeleteShader(FFShader);
    end;
    glDeleteProgram(FProg);
  end;
end;

end.

