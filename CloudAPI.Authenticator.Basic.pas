unit CloudAPI.Authenticator.Basic;

interface

uses
  CloudAPI.IAuthenticator,
  CloudAPI.Request;

type
  TBasicAuthenticator = class(TInterfacedObject, IAuthenticator)
  private
    FPassword: string;
    FUser: string;
  public
    constructor Create(const AUser, APassword: string);
    procedure Authenticate(ARequest: IcaRequest);
    property Password: string read FPassword write FPassword;
    property User: string read FUser write FUser;
  end;

implementation

uses
  CloudAPI.Types,
  CloudAPI.Parameter,
  System.NetEncoding,
  System.Net.URLClient;
{ TBasicAuthenticator }

procedure TBasicAuthenticator.Authenticate(ARequest: IcaRequest);
var
  LParam: TcaParameter;
begin
  LParam.Name := 'Authorization';
  LParam.Value := 'Basic ' + TNetEncoding.Base64.Encode(User + ':' + Password);
  LParam.DefaultValue := '';
  LParam.ParameterType := TcaParameterType.HttpHeader;
  LParam.IsRequired := True;
  ARequest.AddParam(LParam);
end;

constructor TBasicAuthenticator.Create(const AUser, APassword: string);
begin
  FUser := AUser;
  FPassword := APassword;
end;

end.
