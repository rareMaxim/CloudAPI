unit CloudAPI.Client.Sync;

interface

uses
  CloudAPI.Client.Base,
  CloudAPI.Response,
  CloudAPI.Request;

type
  TCloudApiClient = class(TCloudApiClientBase)
  public
    function Execute(ARequest: IcaRequest): IcaResponseBase; overload;
    function Execute<T>(ARequest: IcaRequest): IcaResponse<T>; overload;
    function GroupExecute(ARequests: TArray<IcaRequest>): TArray<IcaResponseBase>; overload;
    function GroupExecute<T>(ARequests: TArray<IcaRequest>): TArray<IcaResponse<T>>; overload;
  end;

implementation

{ TCloudApiClient }

function TCloudApiClient.Execute(ARequest: IcaRequest): IcaResponseBase;
begin
  Result := InternalExecute(ARequest);
end;

function TCloudApiClient.Execute<T>(ARequest: IcaRequest): IcaResponse<T>;
var
  LResult: IcaResponseBase;
begin
  LResult := Execute(ARequest);
  Result := TcaResponse<T>.Create(ARequest, LResult.HttpRequest, LResult.HttpResponse, GetSerializer);
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
