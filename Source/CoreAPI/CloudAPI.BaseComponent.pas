unit CloudAPI.BaseComponent;

interface

uses
  CloudAPI.Logger,
  CloudAPI.Request,
  System.Net.URLClient,
  System.Classes;

type
  TOnReceiveRawData = procedure(ASender: TObject; const AData: string) of object;

  TOnSendData = procedure(ASender: TObject; const AUrl, AData: string) of object;

  ICloudApiBaseComponent = interface
    ['{8E30982C-64EA-4B01-8AD8-AA5A42E15809}']
    function GetLogger: ILogger;
    procedure SetLogger(const Value: ILogger);
    function GetDomain: string;
    procedure SetDomain(const Value: string);
    function GetProxy: TProxySettings;
    procedure SetProxy(const Value: TProxySettings);
    //
    property Logger: ILogger read GetLogger write SetLogger;
    property Domain: string read GetDomain write SetDomain;
    property Proxy: TProxySettings read GetProxy write SetProxy;
  end;

  TCloudApiBaseComponent = class(TComponent, ICloudApiBaseComponent)
  strict private
    FLog: ILogger;
    FRequest: IApiRequest;
    FOnRawData: TOnReceiveRawData;
    FOnSendData: TOnSendData;
  private
    function GetLogger: ILogger;
    procedure SetLogger(const Value: ILogger);
    function GetDomain: string;
    procedure SetDomain(const Value: string);
    function GetProxy: TProxySettings;
    procedure SetProxy(const Value: TProxySettings);
  protected
    function GetRequest: IApiRequest;
    procedure SetRequest(const Value: IApiRequest);
    procedure DoInitApiCore; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    {$REGION 'Property|Свойства'}
    property Logger: ILogger read GetLogger write SetLogger;
    property Domain: string read GetDomain write SetDomain;
    property Proxy: TProxySettings read GetProxy write SetProxy;
    {$ENDREGION}
    {$REGION 'События|Events'}
    property OnReceiveRawData: TOnReceiveRawData read FOnRawData write FOnRawData;
    property OnSendData: TOnSendData read FOnSendData write FOnSendData;
    {$ENDREGION}
  end;

implementation

uses
  System.SysUtils,
  CloudAPI.Exception;
{ TTelegramBotBase }

constructor TCloudApiBaseComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoInitApiCore;
end;

procedure TCloudApiBaseComponent.DoInitApiCore;
begin
  FRequest := TApiRequest.Create;
  GetRequest.OnError :=
    procedure(E: Exception)
    begin
      if Assigned(Logger) then
        Logger.Error('RequestAPI', ECloudApiException(E))
      else
        raise ECloudApiException(E);
    end;
  GetRequest.OnDataReceiveAsString :=
    function(AData: string): string
    begin
      if Assigned(OnReceiveRawData) then
        OnReceiveRawData(Self, AData);
      result := AData;
    end;
  GetRequest.OnDataSend :=
    procedure(AUrl, AData, AHeaders: string)
    begin
      if Assigned(OnSendData) then
        OnSendData(Self, AUrl, AData);
    end;
end;

function TCloudApiBaseComponent.GetLogger: ILogger;
begin
  Result := FLog;
end;

function TCloudApiBaseComponent.GetProxy: TProxySettings;
begin
  Result := GetRequest.HttpClient.ProxySettings;
end;

function TCloudApiBaseComponent.GetRequest: IApiRequest;
begin
  Result := FRequest;
end;

function TCloudApiBaseComponent.GetDomain: string;
begin
  Result := GetRequest.Domain;
end;

procedure TCloudApiBaseComponent.SetLogger(const Value: ILogger);
begin
  FLog := Value;
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

