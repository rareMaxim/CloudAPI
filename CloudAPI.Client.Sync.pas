unit CloudAPI.Client.Sync;

interface

uses
  CloudAPI.Client.Base,
  CloudAPI.Response,
  CloudAPI.Request;

type
  TCloudApiClient = class(TCloudApiClientBase)
    function Execute(ARequest: IcaRequest): IcaResponseBase; overload;
    function Execute<T>(ARequest: IcaRequest): IcaResponse<T>; overload;
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

end.
