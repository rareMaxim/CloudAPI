unit Unit1;

interface

uses
  System.Classes,
  TelegaPI.Bot,
  TelegaPI.Types;

type
  TTelegramRSS = Class(TComponent)
  private
    FUrlRSS: String;
    FBot: TTelegramBot;
    FIsEnabled: Boolean;
    FID: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property TargetID: string read FID write FID;
    property IsEnabled: Boolean read FIsEnabled write FIsEnabled default False;
    property Bot: TTelegramBot read FBot write FBot;
    property UrlRSS: String read FUrlRSS write FUrlRSS;
  End;

implementation

{ TTelegramRSS }

constructor TTelegramRSS.Create(AOwner: TComponent);
begin
  inherited;
  IsEnabled := False;
  UrlRSS := '';

end;

destructor TTelegramRSS.Destroy;
begin

  inherited;
end;

end.
