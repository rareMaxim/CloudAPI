unit CloudAPI.Client.Sync;

interface

uses
  CloudAPI.Client.Base,
  CloudAPI.Response,
  CloudAPI.Request,
  CloudAPI.Types;

type
  TCloudApiClient = class(TCloudApiClientBase)
  public
    function Download(const AUrl, AFileName: string; ARequest: IcaRequest = nil): IcaResponseBase;
    function Execute(ARequest: IcaRequest): IcaResponseBase; overload;
    function Execute<T>(ARequest: IcaRequest): IcaResponse<T>; overload;
    function TryExecute(ARequest: IcaRequest; var AResp: IcaResponseBase): Boolean; overload;
    function TryExecute<T>(ARequest: IcaRequest; var AResp: IcaResponse<T>): Boolean; overload;
    function GroupExecute(ARequests: TArray<IcaRequest>): TArray<IcaResponseBase>; overload;
    function GroupExecute<T>(ARequests: TArray<IcaRequest>): TArray<IcaResponse<T>>; overload;
  end;

implementation

uses
  System.Classes;

{ TCloudApiClient }

function TCloudApiClient.Download(const AUrl, AFileName: string; ARequest: IcaRequest = nil): IcaResponseBase;
begin
  ResponseStream := TFileStream.Create(AFileName, fmCreate);
  try
    BaseUrl := AUrl;
    TryInternalExcecute(ARequest, Result);
  finally
    ResponseStream.Free;
  end;
end;

function TCloudApiClient.Execute(ARequest: IcaRequest): IcaResponseBase;
begin
  TryInternalExcecute(ARequest, Result);
end;

function TCloudApiClient.Execute<T>(ARequest: IcaRequest): IcaResponse<T>;
var
  LResult: IcaResponseBase;
begin
  LResult := Execute(ARequest);
  Result := TcaResponse<T>.Create(ARequest, LResult.HttpRequest, LResult.HttpResponse, GetSerializer,
    LResult.Exception);
end;

function TCloudApiClient.GroupExecute(ARequests: TArray<IcaRequest>): TArray<IcaResponseBase>;
var
  I: Integer;
begin
  SetLength(Result, Length(ARequests));
  for I := Low(ARequests) to High(ARequests) do
    Result[I] := Execute(ARequests[I]);
end;

function TCloudApiClient.GroupExecute<T>(ARequests: TArray<IcaRequest>): TArray<IcaResponse<T>>;
var
  I: Integer;
begin
  SetLength(Result, Length(ARequests));
  for I := Low(ARequests) to High(ARequests) do
    Result[I] := Execute<T>(ARequests[I]);
end;

function TCloudApiClient.TryExecute(ARequest: IcaRequest; var AResp: IcaResponseBase): Boolean;
begin
  Result := TryInternalExcecute(ARequest, AResp);
end;

function TCloudApiClient.TryExecute<T>(ARequest: IcaRequest; var AResp: IcaResponse<T>): Boolean;
var
  LResult: IcaResponseBase;
begin
  if TryExecute(ARequest, LResult) then
    AResp := TcaResponse<T>.Create(ARequest, LResult.HttpRequest, LResult.HttpResponse, GetSerializer,
      LResult.Exception);
  Result := AResp <> nil;
end;

end.
