unit HttpBinTest.Types;

interface

uses
  System.Generics.Collections,
  System.JSON.Converters,
  System.JSON.Serializers;

type
  TJsonStringStringDictionaryConverter = class(TJsonStringDictionaryConverter<string>);

  TTestResponseBody = class
  private
    [JsonName('url')]
    Furl: string;
    [JsonName('origin')]
    Forigin: string;
    [JsonName('headers')]
    [JsonConverter(TJsonStringStringDictionaryConverter)]
    FHeaders: TDictionary<string, string>;
  public
    property Url: string read Furl write Furl;
    property Origin: string read Forigin write Forigin;
    property Headers: TDictionary<string, string> read FHeaders write FHeaders;
  end;

implementation

end.
