{- |
Copyright 2022 Juspay Technologies Pvt Ltd

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Module      :  Beckn.External.MyValueFirst.Flow
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.External.MyValueFirst.Flow where

import qualified Beckn.External.MyValueFirst.API as API
import Beckn.External.MyValueFirst.Types (SubmitSms (..), SubmitSmsRes (..))
import Beckn.Sms.Config (SmsConfig (..))
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified Data.Text as T
import EulerHS.Prelude

submitSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  SubmitSms ->
  m SubmitSmsRes
submitSms url params = do
  callAPI url (API.submitSms params) "submitSms"
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_SEND_SMS") url)

type OtpTemplate = Text

constructOtpSms :: Text -> Text -> OtpTemplate -> Text
constructOtpSms otp hash =
  let otpTemp = "{#otp#}"
      hashTemp = "{#hash#}"
   in T.replace otpTemp otp . T.replace hashTemp hash

type OrgName = Text

type InviteTemplate = Text

constructInviteSms :: OrgName -> InviteTemplate -> Text
constructInviteSms = T.replace "{#org#}"

sendOTP ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  SmsConfig ->
  Text ->
  Text ->
  Text ->
  m SubmitSmsRes
sendOTP smsCfg otpSmsTemplate phoneNumber otpCode = do
  let smsCred = smsCfg.credConfig
  let url = smsCfg.url
  let otpHash = smsCred.otpHash
  submitSms
    url
    SubmitSms
      { username = smsCred.username,
        password = smsCred.password,
        from = smsCfg.sender,
        to = phoneNumber,
        text = constructOtpSms otpCode otpHash otpSmsTemplate
      }

sendSms ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  SmsConfig ->
  Text ->
  Text ->
  m SubmitSmsRes
sendSms smsCfg smsTemplate phoneNumber = do
  let smsCred = smsCfg.credConfig
      url = smsCfg.url
  submitSms
    url
    SubmitSms
      { username = smsCred.username,
        password = smsCred.password,
        from = smsCfg.sender,
        to = phoneNumber,
        text = smsTemplate
      }

checkSmsResult :: (Log m, MonadThrow m) => SubmitSmsRes -> m ()
checkSmsResult =
  \case
    Sent -> pure ()
    BadNumber -> throwError SMSInvalidNumber
    InvalidReceiver -> throwError SMSInvalidNumber
    err -> throwError $ SMSError err
