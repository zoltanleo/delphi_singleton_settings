unit Arm.Settings;

interface
uses
  Vcl.Forms
  , System.IniFiles
  , System.SysUtils
  ;

type
  TSenderObject = (soForm1, soForm2);

//  property SenderObject: TSenderObject read FSenderObject write FSenderObject;
//  TSettings = TClass


  procedure LoadSettings(Sender: TObject);
  procedure SaveSettings(Sender: TObject);

var
  SenderObject: TSenderObject;

implementation


procedure LoadSettings(Sender: TObject);
var
  SettingsFile : TCustomIniFile;
begin
  if not TObject(Sender).InheritsFrom(TCustomForm) then Exit;

  { Open an instance }
  SettingsFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));

  try
    {
    Read all saved values from the last session. The section name
    is the name of the form. Also use the form's properties as defaults.
    }
    with TCustomForm(Sender) do
    begin
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
    end;
  finally
    SettingsFile.Free;
  end;
end;

procedure SaveSettings(Sender: TObject);
var
  SettingsFile : TCustomIniFile;
begin
  if not TObject(Sender).InheritsFrom(TCustomForm) then Exit;

  { Open an instance }
  SettingsFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));
  try
    with TCustomForm(Sender) do
    begin
      SettingsFile.WriteInteger (Name, 'Top', Top);
      SettingsFile.WriteInteger (Name, 'Left', Left);
      SettingsFile.WriteInteger (Name, 'Width', Width);
      SettingsFile.WriteInteger (Name, 'Height', Height);
      SettingsFile.WriteString  (Name, 'Caption', Caption);
      SettingsFile.WriteBool    (Name, 'InitMax', WindowState = wsMaximized );
//      SettingsFile.WriteDateTime(Name, 'LastRun', Now);
    end;

    case SenderObject of
      soForm1:;
      soForm2: SettingsFile.WriteDateTime(TCustomForm(Sender).Name, 'LastRun', Now);
    end;
  finally
//    if RadioGroup1.ItemIndex = 2 then
    SettingsFile.UpdateFile;

    SettingsFile.Free;
  end;
end;

end.
