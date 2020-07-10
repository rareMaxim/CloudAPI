unit CloudAPI.Exceptions;

interface

uses
  CloudAPI.Parameter,
  System.SysUtils;

type
  ECloudApiException = class(Exception);

  ECloudApiRequairedParameterException = class(ECloudApiException)
  private const
    C_MESSAGE = 'Parameter %s is requaired!';
  public
    constructor Create(AParameter: TcaParameter);
  end;

implementation

{ ECloudApiRequairedParameterException }

constructor ECloudApiRequairedParameterException.Create(AParameter: TcaParameter);
begin
  inherited CreateFmt(C_MESSAGE, [AParameter.Name]);
end;

end.
