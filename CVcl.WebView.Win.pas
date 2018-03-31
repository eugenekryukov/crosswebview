{
  This file is part of CrossVCL WebBroser 
  Copyright (c) 2018 Eugene Kryukov

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}

unit CVcl.WebView.Win;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls, CVcl.WebView, SHDocVw;

type

  TWinWebView = class(TInterfacedObject, IWebView)
  private
    FWebBrowser: TWebBrowser;
    FParentWnd: HWnd;
    FDelegate: IWebViewDelegate;
    procedure DocumentComplete(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    { IWebView }
    procedure WebViewUpdateBounds;
    procedure WebViewNavigate(const URL: string);
    procedure WebViewGoForward;
    procedure WebViewGoBack;
    function WebGetHandle: Pointer;
  protected
  public
    constructor Create(AParentWnd: HWnd; ADelegate: IWebViewDelegate);
    destructor Destroy; override;
  end;

implementation

{ TWinWebView }

constructor TWinWebView.Create(AParentWnd: HWnd; ADelegate: IWebViewDelegate);
begin
  inherited Create;
  FDelegate := ADelegate;
  FParentWnd := AParentWnd;
  FWebBrowser := TWebBrowser.Create(nil);
  FWebBrowser.ParentWindow := FParentWnd;
  FWebBrowser.OnDocumentComplete := DocumentComplete;
  WebViewUpdateBounds;
end;

destructor TWinWebView.Destroy;
begin
  FWebBrowser.DisposeOf;
  inherited;
end;

procedure TWinWebView.DocumentComplete(ASender: TObject; const pDisp: IDispatch;
  const URL: OleVariant);
begin
  if Assigned(FDelegate) then
    FDelegate.FinishLoading;
end;

function TWinWebView.WebGetHandle: Pointer;
begin
  Result := FWebBrowser;
end;

procedure TWinWebView.WebViewGoBack;
begin
  FWebBrowser.GoForward;
end;

procedure TWinWebView.WebViewGoForward;
begin
  FWebBrowser.GoBack;
end;

procedure TWinWebView.WebViewNavigate(const URL: string);
begin
  FWebBrowser.Navigate(URL);
end;

procedure TWinWebView.WebViewUpdateBounds;
var
  R: TRect;
begin
  GetClientRect(FParentWnd, R);
  with R do
    FWebBrowser.SetBounds(Left, Top, Width, Height);
end;

end.
