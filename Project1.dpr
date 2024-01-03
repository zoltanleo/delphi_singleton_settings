program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2},
  Arm.Settings in 'Arm.Settings.pas',
  Arm.Settings.Common in 'Arm.Settings.Common.pas',
  Unit5 in 'Unit5.pas' {Form3},
  Arm.Singleton in 'Arm.Singleton.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
