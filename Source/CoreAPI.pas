unit CoreAPI;

{$I config.inc}
interface

uses
  System.SysUtils,
  System.Rtti,
  CoreAPI.Parameter,
  System.Classes;

type
  ItgRequestAPI = interface
    ['{3DC5A653-F52D-4A31-87AD-0C008AFA7111}']
    // private
    function GetOnError: TProc<Exception>;
    procedure SetOnError(const Value: TProc<Exception>);
    function GetOnSend: TProc<string, string>;
    procedure SetOnSend(const Value: TProc<string, string>);
    function GetOnReceive: TProc<string>;
    procedure SetOnReceive(const Value: TProc<string>);
    function GetDataExtractor: TFunc<string, string>;
    procedure SetDataExtractor(const Value: TFunc<string, string>);
    // public
    function SetToken(const AToken: string): ItgRequestAPI;
    function SetMethod(const AMethod: string): ItgRequestAPI;
    function AddParameter(const AKey: string; AValue, ADefaultValue: TValue; ARequired: Boolean = False): ItgRequestAPI;
    function ClearParameters: ItgRequestAPI;
    function Execute: string;
    function ExecuteAsBool: Boolean;
    function ExecuteAndReadValue: string;
    // props
    property DataExtractor: TFunc<string, string> read GetDataExtractor write SetDataExtractor;
    // events
    property OnError: TProc<Exception> read GetOnError write SetOnError;
    property OnSend: TProc<string, string> read GetOnSend write SetOnSend;
    property OnReceive: TProc<string> read GetOnReceive write SetOnReceive;
  end;

implementation

end.

