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
    FMessage: string;
    FRaisedAt: TDateTime;
  protected
    function BuildMsg: string; virtual;
  public
    constructor Create(const ACode, AMessage: string); overload;
    constructor Create(const ACode: Integer; const AMessage: string); overload;
  published
    property CodeInt: Integer read FCodeInt write FCodeInt;
    property CodeStr: string read FCodeStr write FCodeStr;
    property Message: string read FMessage write FMessage;
    property RaisedAt: TDateTime read FRaisedAt write FRaisedAt;
  end;

  ECloudApiRequairedParameterException = class(ECloudApiException)
  private
    FParameter: TcaParameter;
  protected
    function BuildMsg: string; override;
  public
    constructor Create(AParameter: TcaParameter);
  end;

implementation

uses
  CloudAPI.Core.Constants;
{ ECloudApiRequairedParameterException }

function ECloudApiRequairedParameterException.BuildMsg: string;
begin
  Result := inherited BuildMsg //
    .Replace('{Parameter.Name}', FParameter.Name) //
    .Replace('{Parameter.Value}', FParameter.ValueAsString) //
end;

constructor ECloudApiRequairedParameterException.Create(AParameter: TcaParameter);
begin
  inherited Create('CloudAPI', BuildMsg);
end;

{ ECloudApiException }

constructor ECloudApiException.Create(const ACode, AMessage: string);
begin
  FCodeStr := ACode;
  TryStrToInt(ACode, FCodeInt);
  FMessage := AMessage;
  FRaisedAt := Now;
  inherited Create(BuildMsg);
end;

function ECloudApiException.BuildMsg: string;
var
  LRaisedAtStr: string;
begin
  LRaisedAtStr := FormatDateTime(TcaConstException.RAISED_AT_FORMAT, FRaisedAt, TFormatSettings.Invariant);
  Result := TcaConstException.EXCEPTION_MSG_FORMAT //
    .Replace('{Code}', CodeStr) //
    .Replace('{RaisedAt}', LRaisedAtStr) //
end;

constructor ECloudApiException.Create(const ACode: Integer; const AMessage: string);
begin
  Self.Create(ACode.ToString, AMessage);
end;

end.
