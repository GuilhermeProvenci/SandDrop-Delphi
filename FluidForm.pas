unit FluidForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  UFluid.Constants, UFluid.Engine, UFluid.Renderer;

type
  TForm2 = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    pnlControls: TPanel;
    tbrDiffusion: TTrackBar;
    tbrViscosity: TTrackBar;
    lblDiff: TLabel;
    lblVisc: TLabel;
    btnReset: TButton;
    btnPulse: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnPulseClick(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure tbrDiffusionChange(Sender: TObject);
    procedure tbrViscosityChange(Sender: TObject);
  private
    FEngine: TFluidEngine;
    FRenderer: TFluidRenderer;
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  FEngine := TFluidEngine.Create;
  FRenderer := TFluidRenderer.Create;
  
  tbrDiffusion.Position := 10;
  tbrViscosity.Position := 10;
  
  DoubleBuffered := True;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FEngine.Free;
  FRenderer.Free;
end;

procedure TForm2.tbrDiffusionChange(Sender: TObject);
begin
  FEngine.Diffusion := tbrDiffusion.Position / 100;
  lblDiff.Caption := Format('Difusão: %.2f', [FEngine.Diffusion]);
end;

procedure TForm2.tbrViscosityChange(Sender: TObject);
begin
  FEngine.Viscosity := tbrViscosity.Position / 100;
  lblVisc.Caption := Format('Viscosidade: %.2f', [FEngine.Viscosity]);
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  FEngine.Update(0.1);
  PaintBox1.Invalidate;
end;

procedure TForm2.PaintBox1Paint(Sender: TObject);
begin
  FRenderer.RenderTo(PaintBox1.Canvas, PaintBox1.ClientRect, FEngine.Density);
end;

procedure TForm2.btnPulseClick(Sender: TObject);
begin
  FEngine.AddDensity(FluidMatrixSize div 2, FluidMatrixSize div 2, 50.0);
end;

procedure TForm2.btnResetClick(Sender: TObject);
begin
  FEngine.Reset;
end;

procedure TForm2.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  SimX, SimY: Integer;
begin
  if (ssLeft in Shift) and (PaintBox1.Width > 0) and (PaintBox1.Height > 0) then
  begin
    SimX := (X * FluidMatrixSize) div PaintBox1.Width;
    SimY := (Y * FluidMatrixSize) div PaintBox1.Height;
    FEngine.AddDensity(SimX, SimY, 5.0);
  end;
end;

end.
