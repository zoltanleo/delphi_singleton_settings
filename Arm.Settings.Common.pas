unit Arm.Settings.Common;

interface
uses
    Arm.Singleton
  , Vcl.Forms
  , Vcl.Dialogs
  , System.IniFiles
  , System.SysUtils
  ;

type
  TSenderObject = (soNil, soForm1, soForm2, soForm3);

  {from here https://www.gunsmoker.ru/2011/04/blog-post.html}
  TSettings = class(TSingleton)
  protected
    constructor Create; override;
  public
    destructor Destroy; override;
  private
    FSaveFolder: String;
    FSettingsFile: TCustomIniFile;
    FSenderObject: TSenderObject;
    FRadioItemsIndex: Integer;
  public
    property SaveFolder: String read FSaveFolder;
    property SettingsFile: TCustomIniFile read FSettingsFile;
    property SenderObject: TSenderObject read FSenderObject write FSenderObject;
    property RadioItemsIndex: Integer read FRadioItemsIndex write FRadioItemsIndex;

    procedure Save(Sender: TObject);
    procedure Load(Sender: TObject);
  end;

  function Settings: TSettings;

implementation

  {TSettings}
function Settings: TSettings;
begin
  Result := TSettings.GetInstance;
end;

constructor TSettings.Create;
begin
  inherited Create;
  FSettingsFile:= TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));
  SenderObject:= soNil;
  FRadioItemsIndex:= 0;
end;

destructor TSettings.Destroy;
begin
  FSettingsFile.Free;
  inherited Destroy;
end;

procedure TSettings.Load(Sender: TObject);
var
  SectName: string;
begin
  if not TObject(Sender).InheritsFrom(TCustomForm) then Exit;

  if not Assigned(SettingsFile) then Exit;

  with TCustomForm(Sender) do
  begin
    Top     := SettingsFile.ReadInteger(Name, 'Top', Top );
    Left    := SettingsFile.ReadInteger(Name, 'Left', Left );
    Width   := SettingsFile.ReadInteger(Name, 'Width', Width );
    Height  := SettingsFile.ReadInteger(Name, 'Height', Height );
    Caption := SettingsFile.ReadString (Name, 'Caption', Caption);

    FRadioItemsIndex:= SettingsFile.ReadInteger(Name, 'RadioItemsIndex', RadioItemsIndex);

    case SettingsFile.ReadBool(Name, 'InitMax', WindowState = wsMaximized) of
      true : WindowState := wsMaximized;
      false: WindowState := wsNormal;
    end;
  end;
end;

procedure TSettings.Save(Sender: TObject);
begin
  if not TObject(Sender).InheritsFrom(TCustomForm) then Exit;
  if not Assigned(SettingsFile) then Exit;

  try
    with TCustomForm(Sender) do
    begin
      SettingsFile.WriteInteger (Name, 'Top', Top);
      SettingsFile.WriteInteger (Name, 'Left', Left);
      SettingsFile.WriteInteger (Name, 'Width', Width);
      SettingsFile.WriteInteger (Name, 'Height', Height);
      SettingsFile.WriteString  (Name, 'Caption', Caption);
      SettingsFile.WriteBool    (Name, 'InitMax', WindowState = wsMaximized );
      SettingsFile.WriteDateTime(Name, 'LastRun', Now);
      SettingsFile.WriteInteger(Name, 'RadioItemsIndex', RadioItemsIndex);
    end;
  finally
    SettingsFile.UpdateFile;
  end;
end;

end.
