unit CloudAPI.Exceptions;

interface

uses
  CloudAPI.Parameter,
  System.SysUtils;

type
  ECloudApiException = class(Exception)
  private
    FCodeInt: Integer;
    FCodeStr: string;
    FText: string;
    FRaisedAt: TDateTime;
  protected
    procedure BuildMsg; virtual;
  public
    constructor Create(const ACode, AText: string); overload;
    constructor Create(const ACode: Integer; const AText: string); overload;
  public
    property CodeInt: Integer read FCodeInt write FCodeInt;
    property CodeStr: string read FCodeStr write FCodeStr;
    property Text: string read FText write FText;
    property RaisedAt: TDateTime read FRaisedAt write FRaisedAt;
  end;

  ECloudApiRequairedParameterException = class(ECloudApiException)
  private
    FParameter: TcaParameter;
    FMethod: string;
  protected
    procedure BuildMsg; override;
  public
    constructor Create(const AMethod: string; AParameter: TcaParameter);
  end;

  TcaExceptionManager = class
  private
    class var FCurrent: TcaExceptionManager;
  private
    FOnAlert: TProc<ECloudApiException>;
    FAlertEvent: Boolean;
    FAlertException: Boolean;
  protected
    procedure DoAlertEvent(AException: ECloudApiException);
    procedure DoAlertException(AException: ECloudApiException);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Alert(AException: ECloudApiException); overload;
    procedure Alert(const ACode, AText: string); overload;
    procedure Alert(const ACode: Integer; const AText: string); overload;
    property AlertEvent: Boolean read FAlertEvent write FAlertEvent default True;
    property AlertException: Boolean read FAlertException write FAlertException default False;
    property OnAlert: TProc<ECloudApiException> read FOnAlert write FOnAlert;
  public
    class constructor Create;
    class destructor Destroy;
    class function Current: TcaExceptionManager;
  end;

implementation

uses
  CloudAPI.Core.Constants;
{ ECloudApiRequairedParameterException }

procedure ECloudApiRequairedParameterException.BuildMsg;
begin
  inherited BuildMsg;
  Message := Message //
    .Replace('{Parameter.Name}', FParameter.Name) //
    .Replace('{Parameter.Value}', FParameter.ValueAsString) //
    .Replace('{Method}', FMethod) //
    ;
end;

constructor ECloudApiRequairedParameterException.Create(const AMethod: string; AParameter: TcaParameter);
begin
  FParameter := AParameter;
  FMethod := AMethod;
  inherited Create('CloudAPI', TcaConstException.PARAMETER_REQIRED);
end;

{ ECloudApiException }

constructor ECloudApiException.Create(const ACode, AText: string);
begin
  FCodeStr := ACode;
  TryStrToInt(ACode, FCodeInt);
  FText := AText;
  FRaisedAt := Now;
  inherited Create(Message);
  BuildMsg;
end;

procedure ECloudApiException.BuildMsg;
var
  LRaisedAtStr: string;
begin
  LRaisedAtStr := FormatDateTime(TcaConstException.RAISED_AT_FORMAT, FRaisedAt, TFormatSettings.Invariant);
  Message := TcaConstException.EXCEPTION_MSG_FORMAT //
    .Replace('{Code}', CodeStr) //
    .Replace('{RaisedAt}', LRaisedAtStr) //
    .Replace('{Message}', Text) //
end;

constructor ECloudApiException.Create(const ACode: Integer; const AText: string);
begin
  self.Create(ACode.ToString, AText);
end;

{ TcaExceptionManager }

procedure TcaExceptionManager.Alert(AException: ECloudApiException);
begin
  if AlertEvent then
    DoAlertEvent(AException);
  if AlertException then
    DoAlertException(AException);
end;

procedure TcaExceptionManager.Alert(const ACode: Integer; const AText: string);
begin
  Alert(ECloudApiException.Create(ACode, AText));
end;

procedure TcaExceptionManager.Alert(const ACode, AText: string);
begin
  Alert(ECloudApiException.Create(ACode, AText));
end;

class constructor TcaExceptionManager.Create;
begin
  FCurrent := TcaExceptionManager.Create;
end;

constructor TcaExceptionManager.Create;
begin
  FAlertEvent := True;
  FAlertException := False;
end;

class function TcaExceptionManager.Current: TcaExceptionManager;
begin
  Result := FCurrent;
end;

destructor TcaExceptionManager.Destroy;
begin
  inherited Destroy;
end;

class destructor TcaExceptionManager.Destroy;
begin
  FreeAndNil(FCurrent);
end;

procedure TcaExceptionManager.DoAlertEvent(AException: ECloudApiException);
begin
  if Assigned(OnAlert) then
    OnAlert(AException);
end;

procedure TcaExceptionManager.DoAlertException(AException: ECloudApiException);
begin
  raise AException;
end;

end.
