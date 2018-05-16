unit WB.MainVcl;

interface

uses
  WelcomeBot.Core,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs;

type
  TForm2 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FCore: TWelcomeBot;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  FCore := TWelcomeBot.Create(ExtractFilePath(ParamStr(0)));
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FCore.Free;
end;

end.

