unit Test.SendVideo;

interface

uses
  DUnitX.TestFramework,
  TelegAPI.Bot;

type
  [TestFixture]
  TTestSendVideo = class(TObject)
  strict private
    FBot: ITelegramBot;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure SendVideo;
    [Test]
    procedure SendVideoNote;
  end;

implementation

uses
  CloudAPI.Request,
  TelegAPI.Bot.Impl,
  Test.Config,
  TelegAPI.Types,
  TelegAPI.Types.Enums;

procedure TTestSendVideo.SendVideo;
const
  duration: Int64 = 104;
  width: Int64 = 320;
  height: Int64 = 240;
  caption = 'Moon Landing';
  mimeType = 'video/mp4';
var
  LMessage: ITgMessage;
begin
  LMessage := FBot.SendVideo(TestConfig.UserId, TFileToSend.FromFile(TTestConst.Video.MoonLanding),
    caption, TtgParseMode.Default, True, duration, width, height);
  Assert.AreEqual(TtgMessageType.Video, LMessage.&Type);
  Assert.AreEqual(caption, LMessage.Caption);
  Assert.AreEqual(duration, LMessage.Video.Duration);
  Assert.AreEqual(width, LMessage.Video.Width);
  Assert.AreEqual(height, LMessage.Video.Height);
  Assert.AreEqual(mimeType, LMessage.Video.MimeType);
  Assert.IsNotEmpty(LMessage.Video.Thumb.FileId);
  Assert.IsTrue(LMessage.Video.Thumb.FileSize > 200);
  Assert.IsTrue(LMessage.Video.Thumb.Width > 50);
  Assert.IsTrue(LMessage.Video.Thumb.Height > 50);
end;

procedure TTestSendVideo.SendVideoNote;
const
  duration: Int64 = 28;
  widthAndHeight: Int64 = 240;
var
  LMessage: ITgMessage;
begin
  LMessage := FBot.SendVideoNote(//
    TestConfig.UserId, //
    TFileToSend.FromFile(TTestConst.Video.GoldenRatio), //
    duration, widthAndHeight);
  Assert.AreEqual(TtgMessageType.VideoNote, LMessage.&Type);
  Assert.AreEqual(duration, LMessage.VideoNote.Duration);
  Assert.AreEqual(widthAndHeight, LMessage.VideoNote.Length);
  Assert.IsNotEmpty(LMessage.VideoNote.Thumb.FileId);
  Assert.IsTrue(LMessage.VideoNote.Thumb.FileSize > 200);
  Assert.IsTrue(LMessage.VideoNote.Thumb.Width > 50);
  Assert.IsTrue(LMessage.VideoNote.Thumb.Height > 50);
end;

procedure TTestSendVideo.Setup;
begin
  FBot := TTelegramBot.Create(TestConfig.Token);
end;

procedure TTestSendVideo.TearDown;
begin
  FBot := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSendVideo);

end.

