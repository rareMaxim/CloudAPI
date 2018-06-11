program Zona.Client;

uses
  System.StartUpCopy,
  FMX.Forms,
  ZonaClient.UI in 'ZonaClient.UI.pas' {Form2},
  ZonaAPI in 'ZonaAPI.pas',
  ZonaAPI.Types in 'ZonaAPI.Types.pas',
  ZonaClient.UI.ItemView in 'ZonaClient.UI.ItemView.pas' {UiItemView: TFrame},
  ZonaAPI.Utils in 'ZonaAPI.Utils.pas',
  ZonaClient.UI.Filter in 'ZonaClient.UI.Filter.pas' {UiFilter: TFrame},
  FrameUiManager in 'FrameUiManager.pas',
  ZonaClient.UI.MediaList in 'ZonaClient.UI.MediaList.pas' {UiMediaList: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

