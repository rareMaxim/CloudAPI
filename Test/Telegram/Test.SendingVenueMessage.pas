unit Test.SendingVenueMessage;

interface

uses
  DUnitX.TestFramework,
  TelegAPI.Bot;

type
  [TestFixture]
  TSendingVenueMessageTests = class(TObject)
  strict private
    FBot: ITelegramBot;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Send_Venue;
  end;

implementation

uses
  TelegAPI.Bot.Impl,
  Test.Config,
  TelegAPI.Types,
  TelegAPI.Types.Impl;

procedure TSendingVenueMessageTests.Setup;
begin
  FBot := TTelegramBot.Create(TestConfig.Token);
end;

procedure TSendingVenueMessageTests.TearDown;
begin
end;

procedure TSendingVenueMessageTests.Send_Venue;
const
  Title: string = 'Rubirosa Ristorante';
  Address: string = '235 Mulberry St';
  lat: Single = 40.722728;
  lon: Single = -73.996006;
  FoursquareId: string = '4cc6222106c25481d7a4a047';
var
  LMessage: ITgMessage;
  LVenue: TtgVenue;
begin
  LVenue := TtgVenue.Create('{"title":""}');
  LVenue.Location:=TtgLocation.Create(lon,lat);
  LVenue.Address:=Address;
  LVenue.Title := Title;
  LVenue.FoursquareId := FoursquareId;
  LMessage := FBot.SendVenue(TestConfig.UserId, LVenue);
end;

initialization
  TDUnitX.RegisterTestFixture(TSendingVenueMessageTests);

end.

