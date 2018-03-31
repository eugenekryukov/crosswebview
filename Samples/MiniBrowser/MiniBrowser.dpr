program MiniBrowser;

uses
  Vcl.Forms,
  MiniBrowserFrm in 'MiniBrowserFrm.pas' {Form51};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm51, Form51);
  Application.Run;
end.
