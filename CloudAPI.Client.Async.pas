unit CloudAPI.Client.Async;

interface

uses
  CloudAPI.Client.Base,
  CloudAPI.Response,
  CloudAPI.Request,
  System.Threading;

type
  TCloudApiClient = class(TCloudApiClientBase)
    function Execute(ARequest: IcaRequest): IFuture<IcaResponseBase>; overload;
    function Execute<T>(ARequest: IcaRequest): IFuture<IcaResponse<T>>; overload;
  end;

implementation

{ TCloudApiClient }

function TCloudApiClient.Execute(ARequest: IcaRequest): IFuture<IcaResponseBase>;
begin
  Result := TTask.Future<IcaResponseBase>(
    function: IcaResponseBase
    begin
      Result := InternalExecute(ARequest);
    end);
  Result.Start;
end;

function TCloudApiClient.Execute<T>(ARequest: IcaRequest): IFuture<IcaResponse<T>>;
begin
  Result := TTask.Future < IcaResponse < T >> (
    function: IcaResponse<T>
    var
      LResult: IcaResponseBase;
    begin
      LResult := InternalExecute(ARequest);
      Result := TcaResponse<T>.Create(ARequest, LResult.HttpRequest, LResult.HttpResponse, GetSerializer);
    end);
  Result.Start;
end;

end.
