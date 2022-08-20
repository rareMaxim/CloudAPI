unit CloudAPI.Client;

interface

uses
  CloudAPI.Client.Base,
  CloudAPI.Response,
  CloudAPI.Request,
  CloudAPI.Types,
  System.Classes,
  System.SysUtils;

type
  TCloudApiClient = class(TCloudApiClientBase)
  public
    function Download(const AUrl, AFileName: string; ARequest: IcaRequest = nil): IcaResponseBase; overload;
    function Download(const AUrl: string; AStream: TStream; ARequest: IcaRequest = nil): IcaResponseBase; overload;
    function Execute(ARequest: IcaRequest): IcaResponseBase; overload;
    function Execute<T>(ARequest: IcaRequest): IcaResponse<T>; overload;
    function TryExecute(ARequest: IcaRequest; var AResp: IcaResponseBase): Boolean; overload;
    function TryExecute<T>(ARequest: IcaRequest; var AResp: IcaResponse<T>): Boolean; overload;
    function GroupExecute(ARequests: TArray<IcaRequest>): TArray<IcaResponseBase>; overload;
    function GroupExecute<T>(ARequests: TArray<IcaRequest>): TArray<IcaResponse<T>>; overload;
    // Async
    procedure TryExecuteAsync(ARequest: IcaRequest; AResponse: TProc<Boolean, IcaResponseBase>); overload;
    procedure TryExecuteAsync<T>(ARequest: IcaRequest; AOnResponse: TProc < IcaResponse < T >> ); overload;
  end;

implementation

uses
  CloudAPI.Exceptions;

{ TCloudApiClient }

function TCloudApiClient.Download(const AUrl, AFileName: string; ARequest: IcaRequest = nil): IcaResponseBase;
var
  lFileStream: TFileStream;
begin
  lFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    Result := Download(AUrl, lFileStream, ARequest);
  finally
    lFileStream.Free;
  end;
end;

function TCloudApiClient.Download(const AUrl: string; AStream: TStream; ARequest: IcaRequest = nil): IcaResponseBase;
var
  lOriginalStream: TStream;
  lOriginalUrl: string;
begin
  lOriginalStream := ResponseStream;
  lOriginalUrl := BaseUrl;
  ResponseStream := AStream;
  try
    BaseUrl := AUrl;
    TryInternalExcecute(ARequest, Result);
  finally
    ResponseStream := lOriginalStream;
    BaseUrl := lOriginalUrl;
  end;
end;

function TCloudApiClient.Execute(ARequest: IcaRequest): IcaResponseBase;
begin
  if not TryInternalExcecute(ARequest, Result) then
    raise Result.Exception;
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

procedure TCloudApiClient.TryExecuteAsync(ARequest: IcaRequest; AResponse: TProc<Boolean, IcaResponseBase>);
var
  LThread: TThread;
begin
  LThread := TThread.CreateAnonymousThread(
    procedure
    var
      LResult: Boolean;
      LResponse: IcaResponseBase;
    begin
      LResult := TryExecute(ARequest, LResponse);
      if IsConsole then
      begin
        if Assigned(AResponse) then
          AResponse(LResult, LResponse);
      end
      else
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            if Assigned(AResponse) then
              AResponse(LResult, LResponse);
          end);
      end;
    end);

  LThread.FreeOnTerminate := True;
  LThread.Start;

end;

procedure TCloudApiClient.TryExecuteAsync<T>(ARequest: IcaRequest; AOnResponse: TProc < IcaResponse < T >> );
begin
  TryExecuteAsync(ARequest,
    procedure(AResult: Boolean; AResponse: IcaResponseBase)
    var
      LResponse: IcaResponse<T>;
    begin
      if Assigned(AOnResponse) then
      begin
        LResponse := TcaResponse<T>.Create(ARequest, AResponse.HttpRequest, AResponse.HttpResponse, GetSerializer,
          AResponse.Exception);
        AOnResponse(LResponse);
      end;
    end);
end;

end.
