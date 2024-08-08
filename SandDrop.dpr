program SandDrop;

uses
  Vcl.Forms,
  unit_Sand in 'unit_Sand.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
