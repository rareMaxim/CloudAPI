unit CoreAPI.Base;

interface

uses
  TelegAPI.Base,
  CoreAPI,
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  CoreAPI.Parameter,
  System.Classes;

type
  TtgCoreApiBase = class(TtgAbstractComponent, ItgRequestAPI)
  protected
    const
      SERVER = 'https://api.telegram.org/bot';
  private
    FParameters: TObjectList<TtgApiParameter>;
    FGetOnSend: TProc<string, string>;
    FDataExtractor: TFunc<string, string>;
    FOnReceive: TProc<string>;
    FLastRequestIsOk: Boolean;
    FToken: string;
    FMethod: string;
    FOnError: TProc<Exception>;
    function GetOnError: TProc<Exception>;
    procedure SetOnError(const Value: TProc<Exception>);
    function GetOnSend: TProc<string, string>;
    procedure SetOnSend(const Value: TProc<string, string>);
    function GetOnReceive: TProc<string>;
    procedure SetOnReceive(const Value: TProc<string>);
    function GetDataExtractor: TFunc<string, string>;
    procedure SetDataExtractor(const Value: TFunc<string, string>);
    function GetParameters: TObjectList<TtgApiParameter>;
    procedure SetParameters(const Value: TObjectList<TtgApiParameter>);
    function GetUrl: string;
  protected
    procedure DoHaveException(const AException: Exception);
    function StreamToString(Stream: TStream): string;
  public
    constructor Create;
    function SetToken(const AToken: string): ItgRequestAPI;
    function SetMethod(const AMethod: string): ItgRequestAPI;
    function AddParameter(const AKey: string; AValue, ADefaultValue: TValue; ARequired: Boolean = False): ItgRequestAPI;
    function ClearParameters: ItgRequestAPI;
    function Execute: string; virtual; abstract;
    function ExecuteAsBool: Boolean;
    function ExecuteAndReadValue: string;
    destructor Destroy; override;
  // props
    property LastRequestIsOk: Boolean read FLastRequestIsOk write FLastRequestIsOk;
    property DataExtractor: TFunc<string, string> read GetDataExtractor write SetDataExtractor;
    property Url: string read GetUrl;
    property Parameters: TObjectList<TtgApiParameter> read GetParameters write SetParameters;
  // events
    property OnError: TProc<Exception> read GetOnError write SetOnError;
    property OnSend: TProc<string, string> read GetOnSend write SetOnSend;
    property OnReceive: TProc<string> read GetOnReceive write SetOnReceive;
  end;

implementation

uses
  System.JSON;

{ TtgCoreApiBase }

function TtgCoreApiBase.AddParameter(const AKey: string; AValue, ADefaultValue: TValue; ARequired: Boolean): ItgRequestAPI;
begin
  FParameters.Add(TtgApiParameter.Create(AKey, AValue, ADefaultValue, ARequired));
  Result := Self;
end;

function TtgCoreApiBase.ClearParameters: ItgRequestAPI;
begin
  FParameters.Clear;
  Result := Self;
end;

constructor TtgCoreApiBase.Create;
begin
  inherited Create(nil);
  FParameters := TObjectList<TtgApiParameter>.Create;
end;

destructor TtgCoreApiBase.Destroy;
begin
  FParameters.Free;
  inherited;
end;

procedure TtgCoreApiBase.DoHaveException(const AException: Exception);
begin
  if Assigned(OnError) then
    OnError(AException)
  else
    raise AException;
end;

function TtgCoreApiBase.ExecuteAndReadValue: string;
var
  LJson: TJSONValue;
begin
  LJson := TJSONObject.ParseJSONValue(Execute);
  try
    Result := LJson.Value;
  finally
    LJson.Free;
  end;
end;

function TtgCoreApiBase.ExecuteAsBool: Boolean;
var
  LJson: TJSONValue;
begin
  LJson := TJSONObject.ParseJSONValue(Execute);
  try
    Result := LJson is TJSONTrue;
  finally
    LJson.Free;
  end;
end;

function TtgCoreApiBase.GetDataExtractor: TFunc<string, string>;
begin
  Result := FDataExtractor;
end;

function TtgCoreApiBase.GetOnError: TProc<Exception>;
begin
  Result := FOnError;
end;

function TtgCoreApiBase.GetOnReceive: TProc<string>;
begin
  Result := FOnReceive;
end;

function TtgCoreApiBase.GetOnSend: TProc<string, string>;
begin
  Result := FGetOnSend;
end;

function TtgCoreApiBase.GetParameters: TObjectList<TtgApiParameter>;
begin
  result := FParameters;
end;

function TtgCoreApiBase.GetUrl: string;
begin
  Result := SERVER + FToken + '/' + FMethod;
end;

procedure TtgCoreApiBase.SetDataExtractor(const Value: TFunc<string, string>);
begin
  FDataExtractor := Value;
end;

function TtgCoreApiBase.SetMethod(const AMethod: string): ItgRequestAPI;
begin
  FMethod := AMethod;
  Result := Self;
end;

procedure TtgCoreApiBase.SetOnError(const Value: TProc<Exception>);
begin
  FOnError := Value;
end;

procedure TtgCoreApiBase.SetOnReceive(const Value: TProc<string>);
begin
  FOnReceive := Value;
end;

procedure TtgCoreApiBase.SetOnSend(const Value: TProc<string, string>);
begin
  FGetOnSend := Value;
end;

procedure TtgCoreApiBase.SetParameters(const Value: TObjectList<TtgApiParameter>);
begin
  FParameters := Value;
end;

function TtgCoreApiBase.SetToken(const AToken: string): ItgRequestAPI;
begin
  FToken := AToken;
  Result := Self;
end;

function TtgCoreApiBase.StreamToString(Stream: TStream): string;
var
  LStrings: TStringList;
begin
  LStrings := TStringList.Create;
  try
    Stream.Position := 0;
    LStrings.LoadFromStream(Stream);
    Result := LStrings.Text;
  finally
    LStrings.Free;
  end;
end;

end.

