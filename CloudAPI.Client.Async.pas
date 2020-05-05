unit CloudAPI.Client.Async;

interface

uses
  CloudAPI.Client.Base,
  CloudAPI.Response,
  CloudAPI.Request,
  System.Threading,
  System.SysUtils;

type
  TCloudApiClient = class(TCloudApiClientBase)
    procedure Execute(ARequest: IcaRequest; OnResult: TProc<IcaResponseBase>); overload;
    procedure Execute<T>(ARequest: IcaRequest; OnResult: TProc < IcaResponse < T >> ); overload;
  end;

implementation

uses
  System.Classes;
{ TCloudApiClient }

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
        GetSerializer);
      TThread.Synchronize(nil,
        procedure()
        begin
          if Assigned(OnResult) then
            OnResult(LResponseT);
        end);
    end);
end;

end.
