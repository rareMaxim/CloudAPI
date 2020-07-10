unit CloudAPI.IAuthenticator;

interface

uses
  CloudAPI.Request;

type
  IAuthenticator = interface
    ['{6B4BE99E-20F9-4BF1-8911-634F55ED8062}']
    procedure Authenticate(ARequest: IcaRequest);
  end;

implementation

end.
