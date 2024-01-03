unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList
  , System.IniFiles
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Win.Registry
  ;

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    ActLoadProperty: TAction;
    ActSaveProperty: TAction;
    ActLoadIniMem: TAction;
    ActSaveIniMem: TAction;
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
    procedure ActLoadPropertyExecute(Sender: TObject);
    procedure ActSavePropertyExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActLoadIniMemExecute(Sender: TObject);
    procedure ActSaveIniMemExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function OpenIniFileInstance: TCustomIniFile;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Unit2, Unit5;

procedure TForm1.ActLoadIniMemExecute(Sender: TObject);
var
  SettingsFile : TCustomIniFile;
begin
  if not TObject(Sender).InheritsFrom(TCustomForm) then Exit;
  SettingsFile:= nil;

  { Open an instance. }
  SettingsFile := OpenIniFileInstance();
  if not Assigned(SettingsFile) then Exit;

  try
    {
    Read all saved values from the last session. The section name
    is the name of the form. Also use the form's properties as defaults.
    }
    Caption := SettingsFile.ReadString (Name, 'Caption', Caption);
    { Load last window state. }
    case SettingsFile.ReadBool(Name, 'InitMax', WindowState = wsMaximized) of
      true : WindowState := wsMaximized;
      false:
        begin
          WindowState := wsNormal;

          Top     := SettingsFile.ReadInteger(Name, 'Top', Top );
          Left    := SettingsFile.ReadInteger(Name, 'Left', Left );
          Width   := SettingsFile.ReadInteger(Name, 'Width', Width );
          Height  := SettingsFile.ReadInteger(Name, 'Height', Height );
        end;
    end;

  finally
    SettingsFile.Free;
  end;
end;

procedure TForm1.ActLoadPropertyExecute(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    Top     := Ini.ReadInteger( 'Form', 'Top', 100 );
    Left    := Ini.ReadInteger( 'Form', 'Left', 100 );
    Caption := Ini.ReadString( 'Form', 'Caption', 'New Form' );
    if Ini.ReadBool( 'Form', 'InitMax', false) then
     WindowState := wsMaximized
    else
     WindowState := wsNormal;
  finally
    Ini.Free;
  end;
end;

procedure TForm1.ActSaveIniMemExecute(Sender: TObject);
var
  SettingsFile: TCustomIniFile;
begin
  if not TObject(Sender).InheritsFrom(TCustomForm) then Exit;
  SettingsFile:= nil;

  { Open an instance. }
  SettingsFile := OpenIniFileInstance();
  if not Assigned(SettingsFile) then Exit;

  {
  Store current form properties to be used in later sessions.
  }
  try
    if (WindowState <> wsMaximized) then
    begin
      SettingsFile.WriteInteger (Name, 'Top', Top);
      SettingsFile.WriteInteger (Name, 'Left', Left);
      SettingsFile.WriteInteger (Name, 'Width', Width);
      SettingsFile.WriteInteger (Name, 'Height', Height);
      SettingsFile.WriteString  (Name, 'Caption', Caption);
    end;
    SettingsFile.WriteBool    (Name, 'InitMax', WindowState = wsMaximized );
//    SettingsFile.WriteDateTime(Name, 'LastRun', Now);
  finally
    if RadioGroup1.ItemIndex = 2 then SettingsFile.UpdateFile;

    SettingsFile.Free;
  end;
end;

procedure TForm1.ActSavePropertyExecute(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    Ini.WriteInteger( 'Form', 'Top', Top);
    Ini.WriteInteger( 'Form', 'Left', Left);
    Ini.WriteString( 'Form', 'Caption', Caption );
    Ini.WriteBool( 'Form', 'InitMax', WindowState = wsMaximized );
  finally
    Ini.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  tmpFrm: TForm2;
begin
  tmpFrm:= TForm2.Create(Self);
  try
    tmpFrm.ShowModal;
  finally
    FreeAndNil(tmpFrm);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  tmpFrm: TForm3;
begin
  tmpFrm:= TForm3.Create(Self);

  try
    tmpFrm.ShowModal;
  finally
    FreeAndNil(tmpFrm);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  ActLoadPropertyExecute(Sender);
  ActLoadIniMemExecute(Sender);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
//  ActSavePropertyExecute(Sender);
  ActSaveIniMemExecute(Sender);
end;

function TForm1.OpenIniFileInstance: TCustomIniFile;
begin
  {
  Open/create a new INI file that has the same name as your executable,
  but a INI extension.
  }
  Result:= nil;

  case RadioGroup1.ItemIndex of
    0:
      begin
        { Registry mode selected: in HKEY_CURRENT_USER\Software\... }
        Result := TRegistryIniFile.Create('Software\' + Application.Title);
      end;
    1:
      begin
        { Ini file mode selected }
        Result := TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));
      end;
    2:
      begin
        { Memory based Ini file mode selected }
        Result := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));
      end;
  end;
end;

end.
