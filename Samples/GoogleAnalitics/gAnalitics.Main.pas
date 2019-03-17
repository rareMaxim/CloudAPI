unit gAnalitics.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CloudAPI.BaseComponent,
  CloudAPI.GoogleAnalitics;

type
  TForm4 = class(TForm)
    gaAnalitics1: TgaAnalitics;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

end.
