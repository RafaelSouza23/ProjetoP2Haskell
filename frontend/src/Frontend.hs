{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.Text as T

import Control.Monad.Fix (MonadFix)
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Data.Aeson
import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.


data Pagina = Principal | InserirCli | Clientes | Tipop | InserirUs 
data Acao = PerfilCliente Int | EditarCliente Int | PerfilServico Int | EditarTipo Int | Excluir Int


getPath :: R BackendRoute ->  T.Text
getPath r = renderBackendRoute checFullREnc r

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

getClienteListReq :: XhrRequest ()
getClienteListReq = xhrRequest "GET" (getPath (BackendRoute_ClienteListar :/ ())) def


getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_ServicosListar :/ ())) def

getUserListReq :: XhrRequest ()
getUserListReq = xhrRequest "GET" (getPath (BackendRoute_UsuarioListar :/ ())) def

getClienteReq :: Int -> XhrRequest()
getClienteReq pid = xhrRequest "GET" (getPath (BackendRoute_ClienteBuscar :/ pid)) def

getExcluirCliente :: Int -> XhrRequest()
getExcluirCliente pid = xhrRequest "GET" (getPath (BackendRoute_ClienteDeletar :/ pid)) def

getExcluirTipo :: Int -> XhrRequest()
getExcluirTipo pid = xhrRequest "GET" (getPath (BackendRoute_ServicosDeletar :/ pid)) def

getTipoR :: Int -> XhrRequest()
getTipoR pid = xhrRequest "GET" (getPath (BackendRoute_ServicosBuscar :/ pid)) def

getTipoRq :: Int -> XhrRequest()
getTipoRq pid = xhrRequest "GET" (getPath (BackendRoute_ServicosBuscar2 :/ pid)) def


editCli :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
editCli pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getClienteReq pid) <$> evt))
  mdyn <- return (switchDyn cli)
  dynE <- return ((fromMaybe (Cliente 1 0 "" "" "" "")) <$> mdyn)

  elAttr "div" ("class" =: "col-3 d-flex flex-column") $ do
    el "label" (text "Nome")
    nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteNome dynE)
    el "label" (text "Telefone")
    telefone <-  inputElement $ def & inputElementConfig_setValue .~ (fmap clienteTelefone dynE)
    el "label" (text "Cpf")
    cpf <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteCpf dynE)
    el "label" (text "Endereço")
    endereco <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteEndereco dynE)   
    let cliente = fmap (\((n,t),(c,e)) -> Cliente  1 0 n t c e) (zipDyn (zipDyn (_inputElement_value nome)(_inputElement_value telefone)) (zipDyn (_inputElement_value cpf)(_inputElement_value endereco)))
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Editar")
    let submitBtn = domEvent Click btn
    let clientEvt = tag (current cliente) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$>
              performRequestAsync (sendRequest (BackendRoute_ClienteEditar :/ pid)
              <$> clientEvt))

    return ("Editar: " <> (T.pack $ show pid), reqClienteLista <$ submitBtn)  

pTipo :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pTipo pid = Workflow $ do
  elAttr "div" ("class" =: "ml-5") $ do
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary btn-sm" <> "style" =: "background: #0d6efd;") (text "Mostrar")
    elAttr "p" ("style" =: "font-size:12px;") (text "(Aperte o botão para mostrar as informações)")

    let evt = domEvent Click btn
    cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getClienteReq pid) <$> evt))
    mdyn <- return (switchDyn cli)
    dynE <- return ((fromMaybe (Cliente 1 0 "" "" "" "")) <$> mdyn)

    elAttr "div" ("class" =: "col-3 d-flex flex-column") $ do
      el "label" (text "Nome")
      nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteNome dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      el "label" (text "Telefone")
      telefone <-  inputElement $ def & inputElementConfig_setValue .~ (fmap clienteTelefone dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      el "label" (text "Cpf")
      cpf <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteCpf dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      el "label" (text "Endereço")
      endereco <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteEndereco dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
      id <- numberInputDisable (fmap codigoCliente dynE)
        
      elAttr "div" ("class" =: "col-6 d-flex flex-column") $ do
        
        el "label" (text "Moeda")
        serv <- inputElement def
        el "label" (text "Valor")
        vl <- numberInput
        el "label" (text "Data da Compra")
        date <- inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "date")
          

        let agend = fmap (\((s,v),(d,i))  -> Servicos  1 0 s v d i) (zipDyn  (zipDyn (_inputElement_value serv) (vl))  (zipDyn (_inputElement_value date)(id)))  
        (btn,_) <- elAttr' "button" ("class" =: "btn btn-success mt-3") (text "Comprar Moeda")
        let submitBtn = domEvent Click btn
        let clientEvt = tag (current agend) submitBtn
        _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$>
                  performRequestAsync (sendRequest (BackendRoute_Servicos :/ ())
                  <$> clientEvt))
        
        return ("Editar: " <> (T.pack $ show pid), rTipoCad <$ submitBtn) 

editTipo :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
editTipo pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Servicos)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getTipoRq pid) <$> evt))
  mdyn <- return (switchDyn cli)
  dynE <- return ((fromMaybe (Servicos 1 0 "" 0 "" 0)) <$> mdyn)

  elAttr "div" ("class" =: "col-6 d-flex flex-column") $ do
        el "h3" (text "Tipo")
        el "label" (text "Serviço")
        serv <- inputElement $ def $ inputElementConfig_setValue .~ (fmap servico dynE)
        el "label" (text "Valor")
        vl <- numberInputDyn (fmap valor dynE) 
        el "label" (text "Data do Serviço")
        date <- inputElement $ def & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "date")
        cdmd <- numberInputDisable (fmap codigoServico dynE)
  
        let agend = fmap (\((s,v),(d,i))  -> Servicos  1 0 s v d i) (zipDyn  (zipDyn (_inputElement_value serv) (vl))  (zipDyn (_inputElement_value date)(cdmd)))  
        (btn,_) <- elAttr' "button" ("class" =: "btn btn-success mt-3") (text "Arrumar Moeda")
        let submitBtn = domEvent Click btn
        let clientEvt = tag (current agend) submitBtn
        _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$>
                performRequestAsync (sendRequest (BackendRoute_ClienteEditar :/ pid)
                    <$> clientEvt))

        return ("Editar: " <> (T.pack $ show pid), reqClienteLista <$ submitBtn)


pPCli :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pPCli pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getClienteReq pid) <$> evt))
  mdyn <- return (switchDyn cli)
  dynE <- return ((fromMaybe (Cliente 1 0 "" "" "" "")) <$> mdyn)

  elAttr "div" ("class" =: "col-3 d-flex flex-column") $ do
    el "label" (text "Nome")
    nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteNome dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
    el "label" (text "Telefone")
    telefone <-  inputElement $ def & inputElementConfig_setValue .~ (fmap clienteTelefone dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
    el "label" (text "Cpf")
    cpf <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteCpf dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
    el "label" (text "Endereço")
    endereco <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteEndereco dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
    id <- numberInputDisable (fmap codigoCliente dynE)   
    
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Voltar")
    let submitBtn = domEvent Click btn

    return ("Voltar: " <> (T.pack $ show pid), rTipoCad <$ submitBtn)  

pPCliCod :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pPCliCod pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Mostrar")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getTipoR pid) <$> evt))
  mdyn <- return (switchDyn cli)
  dynE <- return ((fromMaybe (Cliente 1 0 "" "" "" "")) <$> mdyn)

  elAttr "div" ("class" =: "col-3 d-flex flex-column") $ do
    el "label" (text "Nome")
    nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteNome dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
    el "label" (text "Telefone")
    telefone <-  inputElement $ def & inputElementConfig_setValue .~ (fmap clienteTelefone dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
    el "label" (text "Cpf")
    cpf <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteCpf dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
    el "label" (text "Endereço")
    endereco <- inputElement $ def & inputElementConfig_setValue .~ (fmap clienteEndereco dynE) & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("disabled" =: "teste")
    id <- numberInputDisable (fmap codigoCliente dynE)   
    
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Voltar")
    let submitBtn = domEvent Click btn

    return ("Voltar: " <> (T.pack $ show pid), rTipoCad <$ submitBtn)  

pExCli :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pExCli pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-danger" <> "style" =: "background: #bf0010;") (text "Excluir")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getExcluirCliente pid) <$> evt))
  
    
    
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Voltar")
  let submitBtn = domEvent Click btn

  return ("Voltar: " <> (T.pack $ show pid), rTipoCad <$ submitBtn)  

pExTipo :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pExTipo pid = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-danger" <> "style" =: "background: #bf0010;") (text "Excluir")
  let evt = domEvent Click btn
  cli :: Dynamic t (Event t (Maybe Cliente)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getExcluirTipo pid) <$> evt))
  
    
    
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-success") (text "Voltar")
  let submitBtn = domEvent Click btn

  return ("Voltar: " <> (T.pack $ show pid), rTipoCad <$ submitBtn)  

reqListaClient :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
reqListaClient = do
  r <- workflow reqClienteLista
  el "div" (dynText r)



reqCliente :: (DomBuilder t m, Prerender js t m) => m ()
reqCliente = do
  elAttr "div" ("class" =: "d-inline-flex p-2") $ do
    el "label" (text "Nome: ")
    nome <- inputElement def
    el "label" (text "Telefone: ")
    telefone <- inputElement def
    el "label" (text "CPF: ")
    cpf <- inputElement def
    el "label" (text "Endereço: ")
    endereco <- inputElement def
    el "label" (text "Renda: ")
    renda <- inputElement def
    let cliente = fmap (\((n,t),(c,e)) -> Cliente  1 0 n t c e) (zipDyn (zipDyn (_inputElement_value nome)(_inputElement_value telefone)) (zipDyn (_inputElement_value cpf)(_inputElement_value endereco)))
    el "p" (text "")
    (submitBtn,_) <- elAttr' "button" ("class" =: "btn btn-primary btn-sm") (text "Inserir")
    let click = domEvent Click submitBtn
    let clienteEvt = tag (current cliente) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Cliente :/ ()) <$> clienteEvt ))
    return  ()

tbCli :: (PostBuild t m, DomBuilder t m) => Dynamic t Cliente -> m (Event t Acao)
tbCli cl = do
  el "tr" $ do
    el "td" (dynText $ fmap clienteNome cl)
    el "td" (dynText $ fmap clienteTelefone cl)
    el "td" (dynText $ fmap clienteCpf cl)
    el "td" (dynText $ fmap clienteEndereco cl)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-primary" <> "style" =: "background: #0d6efd;") (text "Comprar Moeda")
    let evt = (fmap (const PerfilCliente)) (domEvent Click btn)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-warning" <> "style" =: "background: #f5deb3; margin-left: 15px;") (text "Editar")
    let evt2 = (fmap (const EditarCliente)) (domEvent Click btn)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-danger" <> "style" =: "background: #bf0010; margin-left: 15px;") (text "Excluir")
    let evt3 = (fmap (const Excluir)) (domEvent Click btn)
    return (attachPromptlyDynWith (flip ($)) (fmap codigoCliente cl) (leftmost [evt, evt2,evt3]))

reqClienteLista :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Workflow t m T.Text
reqClienteLista = Workflow $ do
  el "div" $ do
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-success" <> "style" =: "background: #120a8f;") (text "Mostrar")
    let click = domEvent Click btn
    clients :: Dynamic t (Event t (Maybe [Cliente])) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const getClienteListReq <$> click))
    evt <- return (fmap (fromMaybe []) $ switchDyn clients)
    dynCli <- foldDyn (++) [] evt
    tb <- elAttr "table" ("class" =: "table") $ do
      el "thead" $ do
        el "tr" $ do
          elAttr "th" ("scope" =: "col") (text "Nome")
          elAttr "th" ("scope" =: "col") (text "Telefone")
          elAttr "th" ("scope" =: "col") (text "CPF")
          elAttr "th" ("scope" =: "col") (text "Endereço")
          elAttr "th" ("scope" =: "col") (text "Equipamento")


      el "tbody" $ do
        simpleList dynCli tbCli
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Listagem", escolherPag <$> tb')
    where
      escolherPag (PerfilCliente pid) = pTipo pid
      escolherPag (EditarCliente pid) = editCli pid
      escolherPag (Excluir pid) = pExCli pid

rListTipo :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
rListTipo = do 
    r <- workflow rTipoCad
    el "div" (dynText r)

tabTipo :: (PostBuild t m, DomBuilder t m) => Dynamic t Tipo -> m (Event t Acao)
tabTipo ag = do
  el "tr" $ do
    el "td" (dynText $ fmap (T.pack . show . codigoT) ag)
    el "td" (dynText $ fmap nomeT ag)
    el "td" (dynText $ fmap telefoneT ag)
    el "td" (dynText $ fmap cpfT ag)
    el "td" (dynText $ fmap enderecoT ag)
    el "td" (dynText $ fmap servicoT ag)
    el "td" (dynText $ fmap dataT ag)
    el "td" (dynText $ fmap(T.pack . show . valorT) ag)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-warning" <> "style" =: "background: #f5deb3; margin-left: 15px;") (text "Editar")
    let evt = (fmap (const EditarTipo)) (domEvent Click btn)
    (btn,_) <- elAttr' "button" ("class" =: "btn btn-danger" <> "style" =: "background: #bf0010; margin-bottom: 10px; margin-left: 15px;") (text "Vender")
    let evt2 = (fmap (const Excluir)) (domEvent Click btn)
    return (attachPromptlyDynWith (flip ($)) (fmap codigoT ag) (leftmost [evt,evt2]))

rTipoCad :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Workflow t m T.Text
rTipoCad = Workflow $ do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-success" <> "style" =: "background: #120a8f;") (text "Mostrar")
  let click = domEvent Click btn
  agend :: Dynamic t (Event t (Maybe [Tipo])) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> click))
  evt <- return (fmap (fromMaybe []) $ switchDyn agend)
  dynAgenda <- foldDyn (++) [] evt
  tb <- elAttr "table" ("class" =: "table") $ do
    el "thead" $ do
      el "tr" $ do
        elAttr "th" ("scope" =: "col") (text "Codigo Compra")
        elAttr "th" ("scope" =: "col") (text "Nome do Cliente")
        elAttr "th" ("scope" =: "col") (text "Telefone")
        elAttr "th" ("scope" =: "col") (text "CPF")
        elAttr "th" ("scope" =: "col") (text "Endereço")
        elAttr "th" ("scope" =: "col") (text "Moeda")
        elAttr "th" ("scope" =: "col") (text "Data")
        elAttr "th" ("scope" =: "col") (text "Investido")

    el "tbody" $ do
      simpleList dynAgenda tabTipo
  tb' <- return $ switchDyn $ fmap leftmost tb
  return ("Listagem", escolherPag <$> tb')
  where
    escolherPag (EditarTipo pid) = editTipo pid
    escolherPag (Excluir pid) = pExTipo pid


reqUsuario :: ( DomBuilder t m
        , Prerender js t m
        ) => m ()
reqUsuario = do
  elAttr "div" ("class" =: "d-inline-flex p-2") $ do
    el "label" (text "Usuario") 
    username <- inputElement def
    el "label" (text "Senha") 
    senha <- inputElement def 
    let user = fmap (\(u,s) -> Usuario 0 u s) (zipDyn (_inputElement_value username)(_inputElement_value  senha))
    (submitBtn,_) <- elAttr' "button" ("class" =: "btn btn-primary btn-sm") (text "Inserir")
    let click = domEvent Click submitBtn
    let userEvt = tag (current user) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Usuario :/ ()) <$> userEvt ))
    return ()

tabUsuario :: DomBuilder t m => Usuario -> m ()
tabUsuario us = do
  el "tr" $ do
    el "td" (text $ usuarioUsername us)
    el "td" (text $ usuarioSenha us)



reqUsuarioLista :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
reqUsuarioLista = do
  (btn,_) <- elAttr' "button" ("class" =: "btn btn-success" <> "style" =: "background: #120a8f;") (text "Mostrar")
  let click = domEvent Click btn
  users :: Dynamic t (Event t (Maybe [Usuario])) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const getUserListReq <$> click))
  dynUser <- foldDyn (\ps d -> case ps of 
                                Nothing -> []
                                Just s -> d++s) [] (switchDyn users )
  elAttr "table" ("class" =: "table") $ do
    el "thead" $ do
      el "tr" $ do
        elAttr "th" ("scope" =: "col") (text "Usuario")
        elAttr "th" ("scope" =: "col") (text "Senha")

    el "tbody" $ do
      dyn_ (fmap sequence (ffor dynUser (fmap tabUsuario)))


clickli :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickli p t = do
  (ev, _) <- elAttr' "li" ("class" =: "nav-item") (elAttr "a" ("href" =: "#" <> "class" =: "nav-link") (text t))
  return ((\_ -> p) <$> domEvent Click ev)

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = 
  case p of
    Principal -> blank
    InserirCli -> reqCliente
    Clientes -> reqListaClient
    Tipop -> rListTipo
    InserirUs -> reqUsuario

homepag :: (DomBuilder t m , PostBuild t m, MonadHold t m,MonadFix m, Prerender js t m) => m ()
homepag = do
  pagina <- el "div" menu
  dyn_ $ currPag <$> pagina





menu :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menu = do
  evs <- elAttr "ul" ("style" =:"background: #e3e3e3;" ) $ do
    elAttr "nav" ("class" =: "navbar sticky-top navbar-light bg-light" <> "style" =: "background: #ffffff !important;") $ do
      elAttr "div" ("class" =: "container-fluid") $ do
        elAttr "a" ("class" =: "navbar-brand" <> "href" =: "#") (text "Clientes")
        elAttr "div" ("class" =: "navbar-collapse" <> "id" =: "navbarSupporedContent") $ do
          elAttr "ul" ("class" =: "navbar-nav me-auto mb-2 mb-lg-0") $ do
            p2 <- clickli InserirCli "Inserir Cliente"
            p3 <- clickli Clientes "Clientes"
            p4 <- clickli Tipop "Tipo"
            p5 <- clickli InserirUs "Inserir Usuario"
            return (leftmost [p2,p3,p4,p5])
    
  holdDyn Principal evs



numberInput :: (Read a, Num a) => DomBuilder t m => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) $ _inputElement_value n
numberInputDisable :: (DomBuilder t m, Num a, Read a, Show a) => Event t a -> m (Dynamic t a)
numberInputDisable p = do
    val <- return (fmap (T.pack . show) p)
    n <- inputElement $ def
      & inputElementConfig_setValue .~ val
      & inputElementConfig_elementConfig 
      . elementConfig_initialAttributes .~ ("disabled" =: "teste" <> "type" =: "hidden")
    return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                  (_inputElement_value n)

numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) => Event t a -> m (Dynamic t a)
numberInputDyn p = do
    val <- return (fmap (T.pack . show) p)
    n <- inputElement $ def
      & inputElementConfig_setValue .~ val
    return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                  (_inputElement_value n)
      



frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Compra de Moedas"
      elAttr "link" ("href" =: static @"main.css" 
              <> "type" =: "text/css" 
              <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"
                    <> "rel" =: "stylesheet"
                    <> "integrity" =: "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" 
                    <> "crossorigin" =:"anonymous") blank
      elAttr "script" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"
                    <> "integrity" =: "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" 
                    <> "crossorigin" =:"anonymous") blank
  , _frontend_body = do
      homepag
        
      elAttr "script" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"
                    <> "integrity" =: "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" 
                    <> "crossorigin" =:"anonymous") blank
    
    
     
    
  }
