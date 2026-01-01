unit USand.Renderer;

interface

uses
  Winapi.Windows, Vcl.Graphics, Vcl.Direct2D, Winapi.D2D1, System.SysUtils,
  USand.Constants;

type
  TSandRenderer = class
  private
    FRenderBitmap: TBitmap;
    function D2DColor(Color: TColor; Alpha: Single = 1.0): D2D1_COLOR_F;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure RenderTo(
      Canvas: TCanvas; 
      Rect: TRect; 
      const SandBuffer: TMatrixBuffer; 
      const ColorBuffer: TColorBuffer
    );
  end;

implementation

type
  TRGBQuad = record
    B, G, R, A: Byte;
  end;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..32767] of TRGBQuad;

constructor TSandRenderer.Create;
begin
  FRenderBitmap := TBitmap.Create;
  FRenderBitmap.SetSize(MatrixSize, MatrixSize);
  FRenderBitmap.PixelFormat := pf32bit;
end;

destructor TSandRenderer.Destroy;
begin
  FRenderBitmap.Free;
  inherited;
end;

function TSandRenderer.D2DColor(Color: TColor; Alpha: Single): D2D1_COLOR_F;
begin
  Result.r := GetRValue(Color) / 255;
  Result.g := GetGValue(Color) / 255;
  Result.b := GetBValue(Color) / 255;
  Result.a := Alpha;
end;

procedure TSandRenderer.RenderTo(
  Canvas: TCanvas; 
  Rect: TRect; 
  const SandBuffer: TMatrixBuffer; 
  const ColorBuffer: TColorBuffer
);
var
  LCanvas: TDirect2DCanvas;
  X, Y, val: Integer;
  LinePtr: PRGBQuadArray;
  LColor: TColor;
begin
  if (Length(SandBuffer) = 0) or (Length(ColorBuffer) = 0) then Exit;

  // Optimized Scanline processing
  for Y := 0 to MatrixSize - 1 do
  begin
    LinePtr := FRenderBitmap.ScanLine[Y];
    for X := 0 to MatrixSize - 1 do
    begin
      val := SandBuffer[Y * MatrixSize + X];
      if val > 0 then
      begin
        case val of
          MAT_SAND:  LColor := ColorBuffer[Y * MatrixSize + X];
          MAT_WATER: LColor := clSkyBlue;
          MAT_STONE: LColor := clDkGray;
          else LColor := clWhite;
        end;
        
        LinePtr[X].R := GetRValue(LColor);
        LinePtr[X].G := GetGValue(LColor);
        LinePtr[X].B := GetBValue(LColor);
        LinePtr[X].A := 255;
      end
      else
      begin
        LinePtr[X].R := 255;
        LinePtr[X].G := 255;
        LinePtr[X].B := 255;
        LinePtr[X].A := 255;
      end;
    end;
  end;

  LCanvas := TDirect2DCanvas.Create(Canvas, Rect);
  LCanvas.BeginDraw;
  try
    LCanvas.RenderTarget.Clear(D2DColor(clWhite));
    LCanvas.StretchDraw(Rect, FRenderBitmap);
  finally
    LCanvas.EndDraw;
    LCanvas.Free;
  end;
end;

end.
