program WebViewStream;

uses
  Vcl.Forms,
  WebViewStreamFrm in 'WebViewStreamFrm.pas' {WebViewForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWebViewForm, WebViewForm);
  Application.Run;
end.
