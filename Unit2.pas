unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList
  , System.IniFiles, Arm.Settings, Vcl.StdCtrls
  ;

type
  TForm2 = class(TForm)
    ActionList1: TActionList;
    actLoad: TAction;
    actSave: TAction;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actLoadExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FSenderObject: TSenderObject;
    { Private declarations }
  public
    { Public declarations }
    property SenderObject: TSenderObject read FSenderObject;

  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.actLoadExecute(Sender: TObject);
var
  SettingsFile : TCustomIniFile;
begin
  SettingsFile:= TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));
  try
    {
    Read all saved values from the last session. The section name
    is the name of the form. Also use the form's properties as defaults.
    }
    Top     := SettingsFile.ReadInteger(Name, 'Top', Top );
    Left    := SettingsFile.ReadInteger(Name, 'Left', Left );
    Width   := SettingsFile.ReadInteger(Name, 'Width', Width );
    Height  := SettingsFile.ReadInteger(Name, 'Height', Height );
    Caption := SettingsFile.ReadString (Name, 'Caption', Caption);

    { Load last window state. }
    case SettingsFile.ReadBool(Name, 'InitMax', WindowState = wsMaximized) of
      true : WindowState := wsMaximized;
      false: WindowState := wsNormal;
    end;

  finally
    SettingsFile.Free;
  end;
end;

procedure TForm2.actSaveExecute(Sender: TObject);
var
  SettingsFile: TCustomIniFile;
begin
  SettingsFile:= TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));

  try
    SettingsFile.WriteInteger (Name, 'Top', Top);
    SettingsFile.WriteInteger (Name, 'Left', Left);
    SettingsFile.WriteInteger (Name, 'Width', Width);
    SettingsFile.WriteInteger (Name, 'Height', Height);
    SettingsFile.WriteString  (Name, 'Caption', Caption);
    SettingsFile.WriteBool    (Name, 'InitMax', WindowState = wsMaximized );
    SettingsFile.WriteDateTime(Name, 'LastRun', Now);
  finally
    SettingsFile.Free;
  end;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings(Sender);
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  Arm.Settings.SenderObject:= soForm2;
  SaveSettings(Sender);
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  actLoadExecute(Sender);
end;

end.
