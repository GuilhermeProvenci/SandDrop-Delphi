program SandDrop;

uses
  Vcl.Forms,
  unit_Sand in 'unit_Sand.pas' {Form1},
  USand.Constants in 'USand.Constants.pas',
  USand.Physics.Types in 'USand.Physics.Types.pas',
  USand.Physics.Utils in 'USand.Physics.Utils.pas',
  USand.Physics.Sand in 'USand.Physics.Sand.pas',
  USand.Physics.Water in 'USand.Physics.Water.pas',
  USand.Physics.Stone in 'USand.Physics.Stone.pas',
  USand.Physics in 'USand.Physics.pas',
  USand.Renderer in 'USand.Renderer.pas',
  USand.Engine in 'USand.Engine.pas',
  Vcl.Themes,
  Vcl.Styles,
  FluidForm in 'FluidForm.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  //Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
