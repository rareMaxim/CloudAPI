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
  System.Math,
  TelegAPI.Bot.Impl,
  Test.Config,
  TelegAPI.Types,
  TelegAPI.Types.Impl,
  TelegAPI.Types.Enums;

type
  AssertHelper = class helper for Assert
    class procedure InRange(const AValue, AMin, AMax: Double);
  end;

procedure TSendingVenueMessageTests.Setup;
begin
  FBot := TTelegramBot.Create(TestConfig.Token);
end;

procedure TSendingVenueMessageTests.TearDown;
begin
  (FBot as TTelegramBot).Free;
  FBot := nil;
end;

procedure TSendingVenueMessageTests.Send_Venue;
const
  Title: string = 'Rubirosa Ristorante';
  Address: string = '235 Mulberry St';
  lat: Double = 46.661372;
  lon: Double = 36.0926739;
  FoursquareId: string = '4cc6222106c25481d7a4a047';
var
  LMessage: ITgMessage;
  LVenue: TtgVenue;
begin
  LVenue := TtgVenue.Create('{"title":""}');
  LVenue.Location := TtgLocation.Create(lon, lat);
  LVenue.Address := Address;
  LVenue.Title := Title;
  LVenue.FoursquareId := FoursquareId;
  LMessage := FBot.SendVenue(TestConfig.UserId, LVenue);

  Assert.AreEqual(TtgMessageType.Venue, LMessage.&Type);
  Assert.AreEqual(Title, LMessage.Venue.Title);
  Assert.AreEqual(Address, LMessage.Venue.Address);
  Assert.AreEqual(FoursquareId, LMessage.Venue.FoursquareId);
  Assert.InRange(LMessage.Venue.Location.Latitude, lat - 0.001, lat + 0.001);
  Assert.InRange(LMessage.Venue.Location.Longitude, lon - 0.001, lon + 0.001);
end;

{ AssertHelper }

class procedure AssertHelper.InRange(const AValue, AMin, AMax: Double);
begin
  DoAssert;
  if not System.Math.InRange(AValue, AMin, AMax) then
    Fail('AValue not in range', ReturnAddress);
end;

initialization
  TDUnitX.RegisterTestFixture(TSendingVenueMessageTests);

end.

