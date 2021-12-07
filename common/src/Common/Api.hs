{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Api where
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

data Usuario = Usuario {
    codigoUsuario :: Int,
    usuarioUsername :: Text,
    usuarioSenha :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Cliente = Cliente {
    cdUsuarioCliente :: Int,
    codigoCliente :: Int,
    clienteNome :: Text,
    clienteTelefone :: Text,
    clienteCpf :: Text,
    clienteEndereco :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Cidade = Cidade {
    cdCid :: Int,
    cidade :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Servicos = Servicos {
    cdUsuario :: Int,
    codigoServico :: Int,
    servico :: Text,
    valor :: Double,
    date :: Text,
    idcategoria :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data ServicosEdit = ServicosEdit {
    cdUsuarioServ :: Int,
    codigoServicoServ :: Int,
    servicoName :: Text,
    valorServ :: Double,
    dateServ :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Tipo = Tipo {
    cdUsuarioT :: Int,
    codigoT :: Int,
    servicoT :: Text,
    valorT :: Double,
    dataT :: Text,
    idcategoriaT :: Int,
    nomeT :: Text,
    telefoneT :: Text,
    cpfT :: Text,
    enderecoT :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)
