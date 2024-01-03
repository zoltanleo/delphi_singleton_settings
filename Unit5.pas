unit Unit5;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Arm.Settings.Common
  ;

type
  TForm3 = class(TForm)
    Edit1: TEdit;
    RadioGroup1: TRadioGroup;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FRadioItemsIndex: Integer;
    { Private declarations }
  public
    { Public declarations }
    property RadioItemsIndex: Integer read FRadioItemsIndex write FRadioItemsIndex;
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Settings.SenderObject:= soForm3;
  Settings.RadioItemsIndex:= RadioGroup1.ItemIndex;
  Settings.Save(Self);
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  Settings.SenderObject:= soForm3;
  Settings.Load(Self);
  RadioGroup1.ItemIndex:= Settings.RadioItemsIndex;
end;

end.
