unit gAnalitics.Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  CloudAPI.BaseComponent,
  CloudAPI.GoogleAnalitics,
  CloudAPI.GoogleAnalitics.Extensions,
  Vcl.AppAnalytics,
  Vcl.StdCtrls;

type
  TForm4 = class(TForm)
    gaAnalitics1: TgaAnalitics;
    AppAnalytics1: TAppAnalytics;
    mmo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gaAnalitics1SendData(ASender: TObject; const AUrl, AData: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.FormCreate(Sender: TObject);
begin
  gaAnalitics1.Session.SessionController := TgaSessionControllerState.Start;
  gaAnalitics1.pageview('main');
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  gaAnalitics1.Session.SessionController := TgaSessionControllerState.&End;
  gaAnalitics1.pageview('main');
end;

procedure TForm4.gaAnalitics1SendData(ASender: TObject; const AUrl, AData: string);
begin
  mmo1.Lines.AddPair(AUrl, AData)
end;

end.

