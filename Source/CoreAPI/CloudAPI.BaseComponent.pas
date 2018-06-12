unit CloudAPI.BaseComponent;

interface

uses
  CloudAPI.Logger,
  CloudAPI.Request,
  System.Classes;

type
  TOnReceiveRawData = procedure(ASender: TObject; const AData: string) of object;

  TOnSendData = procedure(ASender: TObject; const AUrl, AData: string) of object;

  TCloudApiBaseComponent = class(TComponent)
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
  protected
    function GetRequest: IApiRequest;
    procedure SetRequest(const Value: IApiRequest);
    procedure DoInitApiCore; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    {$REGION 'Property|Свойства'}
    property Logger: ILogger read GetLogger write SetLogger;
    property Domain: string read GetDomain write SetDomain;
    {$ENDREGION}
    {$REGION 'События|Events'}
    property OnReceiveRawData: TOnReceiveRawData read FOnRawData write FOnRawData;
    property OnSendData: TOnSendData read FOnSendData write FOnSendData;
    {$ENDREGION}
  end;

implementation

uses
  System.SysUtils;
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
      Logger.Error('RequestAPI', E);
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
  if FLog = nil then
    FLog := TLogEmpty.Create(nil);
  Result := FLog;
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

procedure TCloudApiBaseComponent.SetRequest(const Value: IApiRequest);
begin
  FRequest := Value;
end;

procedure TCloudApiBaseComponent.SetDomain(const Value: string);
begin
  GetRequest.Domain := Value;
end;

end.

