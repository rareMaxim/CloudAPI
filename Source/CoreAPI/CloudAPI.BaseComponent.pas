unit CloudAPI.BaseComponent;

interface

uses
  CloudAPI.Request,
  CloudAPI.Exception,
  System.Net.URLClient,
  System.SysUtils,
  System.Classes;

type

{$IFDEF CONSOLE}
  TOnReceiveRawData = TProc<TObject, string>;
  TOnSendData = TProc<TObject, string, string>;
  TOnError = TProc<TObject, ECloudApiException>;
{$ELSE}
  TOnReceiveRawData = procedure(ASender: TObject; const AData: string) of object;
  TOnSendData = procedure(ASender: TObject; const AUrl, AData: string) of object;
  TOnError = procedure(ASender: TObject; const Exception: ECloudApiException) of object;
{$ENDIF}

  TCloudApiBaseComponent = class(TComponent)
  strict private
    FRequest: IApiRequest;
    FOnRawData: TOnReceiveRawData;
    FOnSendData: TOnSendData;
    FOnError: TOnError;
  private
    function GetDomain: string;
    procedure SetDomain(const Value: string);
    function GetProxy: TProxySettings;
    procedure SetProxy(const Value: TProxySettings);
  protected
    function GetRequest: IApiRequest;
    procedure SetRequest(const Value: IApiRequest);
    procedure DoInitApiCore; virtual;
    procedure DoCallLogEvent(AException: ECloudApiException; const ACanBeFree: Boolean); overload;
  public
    constructor Create(AOwner: TComponent); override;
{$REGION 'Property|Свойства'}
    property Domain: string read GetDomain write SetDomain;
    property Proxy: TProxySettings read GetProxy write SetProxy;
{$ENDREGION}
{$REGION 'События|Events'}
    property OnReceiveRawData: TOnReceiveRawData read FOnRawData write FOnRawData;
    property OnSendData: TOnSendData read FOnSendData write FOnSendData;
    property OnError: TOnError read FOnError write FOnError;
{$ENDREGION}
  end;

implementation

{ TTelegramBotBase }

constructor TCloudApiBaseComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoInitApiCore;
end;

procedure TCloudApiBaseComponent.DoCallLogEvent(AException: ECloudApiException; const ACanBeFree: Boolean);
begin
  if Assigned(OnError) then
    OnError(Self, AException)
  else
    raise AException;
  if ACanBeFree then
    FreeAndNil(AException);
end;

procedure TCloudApiBaseComponent.DoInitApiCore;
begin
  FRequest := TApiRequest.Create;
  GetRequest.OnError := procedure(E: ECloudApiException)
    begin
      DoCallLogEvent(ECloudApiException(E), False);
    end;
  GetRequest.OnDataReceiveAsString := function(AData: string): string
    begin
      if Assigned(OnReceiveRawData) then
        OnReceiveRawData(Self, AData);
      result := AData;
    end;
  GetRequest.OnDataSend := procedure(AUrl, AData, AHeaders: string)
    begin
      if Assigned(OnSendData) then
        OnSendData(Self, AUrl, AData);
    end;
end;

function TCloudApiBaseComponent.GetProxy: TProxySettings;
begin
  result := GetRequest.HttpClient.ProxySettings;
end;

function TCloudApiBaseComponent.GetRequest: IApiRequest;
begin
  result := FRequest;
end;

function TCloudApiBaseComponent.GetDomain: string;
begin
  result := GetRequest.Domain;
end;

procedure TCloudApiBaseComponent.SetProxy(const Value: TProxySettings);
begin
  GetRequest.HttpClient.ProxySettings := Value;
end;

procedure TCloudApiBaseComponent.SetRequest(const Value: IApiRequest);
begin
  FRequest := Value;
end;

procedure TCloudApiBaseComponent.SetDomain(const Value: string);
begin
  GetRequest.Domain := Value;
end;

end.
