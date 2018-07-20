unit SendingContactMessageTests;

interface

uses
  DUnitX.TestFramework,
  TelegAPI.Bot;

type
  [TestFixture]
  TSendingContactMessageTests = class(TObject)
  strict private
    FBot: TTelegramBot;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Send_Contact;
  end;

implementation

uses
  Test.Config,
  TelegAPI.Types,
  TelegAPI.Types.Impl,
  TelegAPI.Types.Enums;

procedure TSendingContactMessageTests.Setup;
begin
  FBot := TTelegramBot.Create(TestConfig.Token);
end;

procedure TSendingContactMessageTests.TearDown;
begin
  (FBot as TTelegramBot).Free;
  FBot := nil;
end;

procedure TSendingContactMessageTests.Send_Contact;
const
  phoneNumber = '+1234567890';
  firstName = 'Han';
  lastName = 'Solo';
var
  LMessage: ITgMessage;
begin
  LMessage := FBot.SendContact(TestConfig.UserId, TtgContact.Create(firstName, lastName, phoneNumber));
  Assert.AreEqual(TtgMessageType.Contact, LMessage.&Type);
  Assert.AreEqual(phoneNumber, LMessage.Contact.PhoneNumber);
  Assert.AreEqual(firstName, LMessage.Contact.FirstName);
  Assert.AreEqual(lastName, LMessage.Contact.LastName);
end;

initialization
  TDUnitX.RegisterTestFixture(TSendingContactMessageTests);

end.

