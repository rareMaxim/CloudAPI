unit CloudAPI.Core.Constants;

interface

type
  TcaConstException = class
  public const
    PARAMETER_REQIRED = 'Parameter "%s" is requaired!';
    RAISED_AT_FORMAT = 'DD:MM:YYYY';
    EXCEPTION_MSG_FORMAT = '({RaisedAt}) [{Code}] {Message}';
  end;

implementation

end.
