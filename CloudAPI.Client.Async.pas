unit CloudAPI.Client.Async;

interface

uses
  CloudAPI.Client.Base,
  CloudAPI.Response,
  CloudAPI.Request,
  System.Threading,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  TCloudApiClient = class(TCloudApiClientBase)
  private
    class var FTask: TList<ITask>;
  protected
  public
    procedure Download(const AUrl, AFileName: string; ARequest: IcaRequest = nil;
      OnResult: TProc < IcaResponseBase >= nil);

    procedure Execute(ARequest: IcaRequest; OnResult: TProc<IcaResponseBase>); overload;
    procedure Execute<T>(ARequest: IcaRequest; OnResult: TProc < IcaResponse < T >> ); overload;
    procedure GroupExecute(ARequests: TArray<IcaRequest>; OnResult: TProc < TArray < IcaResponseBase >> ); overload;
    procedure GroupExecute<T>(ARequests: TArray<IcaRequest>; OnResult: TProc < TArray < IcaResponse<T> >> ); overload;
    class constructor Create;
    class destructor Destroy;
  end;

implementation

{ TCloudApiClient }

class constructor TCloudApiClient.Create;
begin
  FTask := TList<ITask>.Create;
end;

class destructor TCloudApiClient.Destroy;
begin
  TTask.WaitForAll(FTask.ToArray);
  FTask.Free;
end;

procedure TCloudApiClient.Download(const AUrl, AFileName: string; ARequest: IcaRequest;
  OnResult: TProc<IcaResponseBase>);
var
  LResult: IcaResponseBase;
begin
  ResponseStream := TFileStream.Create(AFileName, fmCreate);
  try
    BaseUrl := AUrl;
    LResult := InternalExecute(ARequest);

  finally
    ResponseStream.Free;
  end;
  TThread.Synchronize(nil,
    procedure()
    begin
      if Assigned(OnResult) then
        OnResult(LResult);
    end);
end;

procedure TCloudApiClient.Execute(ARequest: IcaRequest; OnResult: TProc<IcaResponseBase>);
var
  LTask: ITask;
begin
  LTask := TTask.Run(
    procedure
    var
      LResult: IcaResponseBase;
    begin
      LResult := InternalExecute(ARequest);
      TThread.Synchronize(nil,
        procedure()
        begin
          if Assigned(OnResult) then
            OnResult(LResult);
        end);
    end);
  FTask.Add(LTask);
end;

procedure TCloudApiClient.Execute<T>(ARequest: IcaRequest; OnResult: TProc < IcaResponse < T >> );
var
  LTask: ITask;
begin
  LTask := TTask.Run(
    procedure
    var
      LResponseBase: IcaResponseBase;
      LResponseT: IcaResponse<T>;
    begin
      LResponseBase := InternalExecute(ARequest);
      LResponseT := TcaResponse<T>.Create(ARequest, LResponseBase.HttpRequest, LResponseBase.HttpResponse,
        GetSerializer, nil);
      TThread.Synchronize(nil,
        procedure()
        begin
          if Assigned(OnResult) then
            OnResult(LResponseT);
        end);
    end);
end;

procedure TCloudApiClient.GroupExecute(ARequests: TArray<IcaRequest>; OnResult: TProc < TArray < IcaResponseBase >> );
var
  LTask: ITask;
begin
  LTask := TTask.Run(
    procedure
    var
      LResult: TArray<IcaResponseBase>;
      I: Integer;
    begin
      SetLength(LResult, Length(ARequests));
      for I := Low(ARequests) to High(ARequests) do
      begin
        LResult[I] := InternalExecute(ARequests[I]);
      end;
      TThread.Synchronize(nil,
        procedure()
        begin
          if Assigned(OnResult) then
            OnResult(LResult);
        end);
    end);
end;

procedure TCloudApiClient.GroupExecute<T>(ARequests: TArray<IcaRequest>; OnResult: TProc < TArray < IcaResponse<T> >> );
var
  LTask: ITask;
begin
  LTask := TTask.Run(
    procedure
    var
      LResult: IcaResponseBase;
      LResponseT: TArray<IcaResponse<T>>;
      I: Integer;
    begin
      SetLength(LResponseT, Length(ARequests));
      for I := Low(ARequests) to High(ARequests) do
      begin
        LResult := InternalExecute(ARequests[I]);
        LResponseT[I] := TcaResponse<T>.Create(ARequests[I], LResult.HttpRequest, LResult.HttpResponse,
          GetSerializer, nil);
      end;
      TThread.Synchronize(nil,
        procedure()
        begin
          if Assigned(OnResult) then
            OnResult(LResponseT);

        end);
    end);
end;

end.
