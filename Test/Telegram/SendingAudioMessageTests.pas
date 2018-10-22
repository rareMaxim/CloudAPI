unit SendingAudioMessageTests;

interface

uses
  DUnitX.TestFramework,
  TelegAPI.Bot;

type

  [TestFixture]
  TSendingAudioMessageTests = class(TObject)
  strict private
    FBot: TTelegramBot;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Send_Audio;
    [Test]
    procedure Send_Voice;
  end;

implementation

uses
  Test.Config,
  TelegAPI.Types,
  TelegAPI.Types.Enums;

procedure TSendingAudioMessageTests.Send_Voice;
const
  duration: Int64 = 24;
  caption = 'Test Voice in .ogg format';
var
  LMessage: ITgMessage;
begin
  LMessage := FBot.SendVoice(TestConfig.UserId, TTestConst.Audio.TestOgg, caption, TtgParseMode.default, duration);
  Assert.AreEqual(TtgMessageType.Voice, LMessage.&Type);
  Assert.AreEqual(caption, LMessage.caption);
  Assert.AreEqual(duration, LMessage.Voice.duration);
  Assert.AreEqual('audio/ogg', LMessage.Voice.MimeType);
  Assert.IsNotEmpty(LMessage.Voice.FileId);
  Assert.IsTrue(LMessage.Voice.FileSize > 200);
end;

procedure TSendingAudioMessageTests.Setup;
begin
  FBot := TTelegramBot.Create(TestConfig.Token);
end;

procedure TSendingAudioMessageTests.TearDown;
begin
  (FBot as TTelegramBot).Free;
  FBot := nil;
end;

procedure TSendingAudioMessageTests.Send_Audio;
const
  performer = 'Jackson F. Smith';
  title = 'Cantina Rag';
  duration: Int64 = 201;
  caption = 'Audio File in .mp3 format';
var
  LMessage: ITgMessage;
begin
  LMessage := FBot.SendAudio(TestConfig.UserId, TTestConst.Audio.CantinaRagMp3, '', caption, TtgParseMode.default,
    duration, performer, title);

  Assert.AreEqual(TtgMessageType.Audio, LMessage.&Type);
  Assert.AreEqual(caption, LMessage.caption);
  Assert.AreEqual(performer, LMessage.Audio.performer);
  Assert.AreEqual(title, LMessage.Audio.title);
  Assert.AreEqual(duration, LMessage.Audio.duration);
  Assert.AreEqual('audio/mpeg', LMessage.Audio.MimeType);
  Assert.IsNotEmpty(LMessage.Audio.FileId);
  Assert.IsTrue(LMessage.Audio.FileSize > 200);
end;

initialization

TDUnitX.RegisterTestFixture(TSendingAudioMessageTests);

end.
