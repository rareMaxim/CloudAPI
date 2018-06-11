unit Mobizon;

interface

uses
  System.Net.HttpClient,
  System.Net.URLClient,
  System.SysUtils,
  System.JSON;

type
  TMobizon = class
  private
    FToken: string;
    FHttp: THttpClient;
    FOnError: TProc<Exception>;
  protected
    function GetUri(const APath: string): TUri; virtual;
    function HasError(const AJson: TJSONObject): Boolean;
    procedure ExtractData(const RAW: string; var Return: TJSONObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Token: string read FToken write FToken;
    property OnError: TProc<Exception> read FOnError write FOnError;
  end;

  TmzBalance = record
  public
    balance: single;
    currency: string;
  end;

  TmzUser = class(TMobizon)
  protected
    function GetUri(const APath: string): TURI; override;
  public
    function GetOwnBalance: TmzBalance;
  end;

  TmzMessage = class(TMobizon)
  protected
    function GetUri(const APath: string): TURI; override;
  public
    function SendSMSMessage(const recipient, text: string): string;
  end;

implementation

{ TmzUser }

function TmzUser.GetOwnBalance: TmzBalance;
var
  Resp: IHTTPResponse;
  JS: TJSONObject;
begin
  JS := TJSONObject.Create;
  try
    Resp := FHttp.Get(GetUri('GetOwnBalance').ToString);
    ExtractData(Resp.ContentAsString, JS);
    Result.balance := (JS as TJSONValue).GetValue<single>('balance');
    Result.currency := (JS as TJSONValue).GetValue<string>('currency');
  finally
    JS.Free;
  end;
end;

function TmzUser.GetUri(const APath: string): TURI;
begin
  Result := inherited GetUri('user/');
  Result.Path := Result.Path + APath;
end;

{ TMobizon }

function TMobizon.HasError(const AJson: TJSONObject): boolean;
begin
  Result := False;
  if AJson.GetValue('code') <> TJSONNumber.Create(0) then
  begin
    if Assigned(OnError) then
      OnError(Exception.Create(AJson.GetValue('message').Value));
    Result := True;
  end;
end;

constructor TMobizon.Create;
begin
  FHttp := THTTPClient.Create;
end;

destructor TMobizon.Destroy;
begin
  FHttp.Free;
  inherited;
end;

procedure TMobizon.ExtractData(const RAW: string; var Return: TJSONObject);
begin
  Return := TJSONObject.ParseJSONValue(RAW) as TJSONObject;
//  if HasError(Return) then
//    FreeAndNil(Return)
//  else
  begin
    Return := Return.GetValue('data') as TJSONObject;
  end;
end;

function TMobizon.GetUri(const APath: string): TUri;
begin
  Result := TUri.Create('http://api.mobizon.com/service/' + APath);
  Result.AddParameter('apiKey', Token);
end;

{ TmzMessage }

function TmzMessage.GetUri(const APath: string): TURI;
begin
  Result := inherited GetUri('message/');
  Result.Path := Result.Path + APath;
end;

function TmzMessage.SendSMSMessage(const recipient, text: string): string;
var
  Resp: IHTTPResponse;
  JS: TJSONObject;
  Uri: TURI;
begin
  JS := TJSONObject.Create;
  try
    Uri := GetUri('SendSMSMessage');
    Uri.AddParameter('recipient', recipient);
    Uri.AddParameter('text', text);
    Resp := FHttp.Get(Uri.ToString);
    ExtractData(Resp.ContentAsString, JS);
    Result := JS.ToString;
  finally
    JS.Free;
  end;
end;

end.

