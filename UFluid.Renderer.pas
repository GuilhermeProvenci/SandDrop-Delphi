unit UFluid.Renderer;

interface

uses
  Winapi.Windows, Vcl.Graphics, Vcl.Direct2D, Winapi.D2D1, System.SysUtils, System.Math,
  UFluid.Constants;

type
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..32767] of TRGBQuad;

  TFluidRenderer = class
  private
    FRenderBitmap: TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure RenderTo(
      Canvas: TCanvas; 
      Rect: TRect; 
      const Density: TArray<Single>
    );
  end;

implementation

constructor TFluidRenderer.Create;
begin
  FRenderBitmap := TBitmap.Create;
  FRenderBitmap.SetSize(FluidMatrixSize, FluidMatrixSize);
  FRenderBitmap.PixelFormat := pf32bit;
end;

destructor TFluidRenderer.Destroy;
begin
  FRenderBitmap.Free;
  inherited;
end;

procedure TFluidRenderer.RenderTo(
  Canvas: TCanvas; 
  Rect: TRect; 
  const Density: TArray<Single>
);
var
  LCanvas: TDirect2DCanvas;
  X, Y: Integer;
  LinePtr: PRGBQuadArray;
  Val: Byte;
begin
  if Length(Density) = 0 then Exit;

  // Faster 1D density mapping
  for Y := 0 to FluidMatrixSize - 1 do
  begin
    LinePtr := FRenderBitmap.ScanLine[Y];
    for X := 0 to FluidMatrixSize - 1 do
    begin
      Val := Trunc(Min(Density[Y * FluidMatrixSize + X] * 255, 255));
      // Heatmap: Blue to White
      LinePtr[X].rgbRed := Val;
      LinePtr[X].rgbGreen := Val;
      LinePtr[X].rgbBlue := 255;
      LinePtr[X].rgbReserved := 255;
    end;
  end;

  LCanvas := TDirect2DCanvas.Create(Canvas, Rect);
  LCanvas.BeginDraw;
  try
    LCanvas.StretchDraw(Rect, FRenderBitmap);
  finally
    LCanvas.EndDraw;
    LCanvas.Free;
  end;
end;

end.
