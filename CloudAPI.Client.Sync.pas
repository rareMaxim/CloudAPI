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
    Result := InternalExecute(ARequest);
  finally
    ResponseStream.Free;
  end;
end;

function TCloudApiClient.Execute(ARequest: IcaRequest): IcaResponseBase;
begin
  Result := InternalExecute(ARequest);
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

end.
