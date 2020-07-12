{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Product where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Sum
import Network.AWS.Prelude

-- | Aac Settings
--
-- /See:/ 'aacSettings' smart constructor.
data AacSettings = AacSettings'{_aRawFormat ::
                                !(Maybe AacRawFormat),
                                _aCodingMode :: !(Maybe AacCodingMode),
                                _aProfile :: !(Maybe AacProfile),
                                _aRateControlMode ::
                                !(Maybe AacRateControlMode),
                                _aSampleRate :: !(Maybe Double),
                                _aSpec :: !(Maybe AacSpec),
                                _aBitrate :: !(Maybe Double),
                                _aVbrQuality :: !(Maybe AacVbrQuality),
                                _aInputType :: !(Maybe AacInputType)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AacSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aRawFormat' - Sets LATM / LOAS AAC output for raw containers.
--
-- * 'aCodingMode' - Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. The adReceiverMix setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
--
-- * 'aProfile' - AAC Profile.
--
-- * 'aRateControlMode' - Rate Control Mode.
--
-- * 'aSampleRate' - Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- * 'aSpec' - Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
--
-- * 'aBitrate' - Average bitrate in bits/second. Valid values depend on rate control mode and profile.
--
-- * 'aVbrQuality' - VBR Quality Level - Only used if rateControlMode is VBR.
--
-- * 'aInputType' - Set to "broadcasterMixedAd" when input contains pre-mixed main audio + AD (narration) as a stereo pair.  The Audio Type field (audioType) will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. The values in audioTypeControl and audioType (in AudioDescription) are ignored when set to broadcasterMixedAd. Leave set to "normal" when input does not contain pre-mixed audio + AD.
aacSettings
    :: AacSettings
aacSettings
  = AacSettings'{_aRawFormat = Nothing,
                 _aCodingMode = Nothing, _aProfile = Nothing,
                 _aRateControlMode = Nothing, _aSampleRate = Nothing,
                 _aSpec = Nothing, _aBitrate = Nothing,
                 _aVbrQuality = Nothing, _aInputType = Nothing}

-- | Sets LATM / LOAS AAC output for raw containers.
aRawFormat :: Lens' AacSettings (Maybe AacRawFormat)
aRawFormat = lens _aRawFormat (\ s a -> s{_aRawFormat = a})

-- | Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. The adReceiverMix setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
aCodingMode :: Lens' AacSettings (Maybe AacCodingMode)
aCodingMode = lens _aCodingMode (\ s a -> s{_aCodingMode = a})

-- | AAC Profile.
aProfile :: Lens' AacSettings (Maybe AacProfile)
aProfile = lens _aProfile (\ s a -> s{_aProfile = a})

-- | Rate Control Mode.
aRateControlMode :: Lens' AacSettings (Maybe AacRateControlMode)
aRateControlMode = lens _aRateControlMode (\ s a -> s{_aRateControlMode = a})

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
aSampleRate :: Lens' AacSettings (Maybe Double)
aSampleRate = lens _aSampleRate (\ s a -> s{_aSampleRate = a})

-- | Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
aSpec :: Lens' AacSettings (Maybe AacSpec)
aSpec = lens _aSpec (\ s a -> s{_aSpec = a})

-- | Average bitrate in bits/second. Valid values depend on rate control mode and profile.
aBitrate :: Lens' AacSettings (Maybe Double)
aBitrate = lens _aBitrate (\ s a -> s{_aBitrate = a})

-- | VBR Quality Level - Only used if rateControlMode is VBR.
aVbrQuality :: Lens' AacSettings (Maybe AacVbrQuality)
aVbrQuality = lens _aVbrQuality (\ s a -> s{_aVbrQuality = a})

-- | Set to "broadcasterMixedAd" when input contains pre-mixed main audio + AD (narration) as a stereo pair.  The Audio Type field (audioType) will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. The values in audioTypeControl and audioType (in AudioDescription) are ignored when set to broadcasterMixedAd. Leave set to "normal" when input does not contain pre-mixed audio + AD.
aInputType :: Lens' AacSettings (Maybe AacInputType)
aInputType = lens _aInputType (\ s a -> s{_aInputType = a})

instance FromJSON AacSettings where
        parseJSON
          = withObject "AacSettings"
              (\ x ->
                 AacSettings' <$>
                   (x .:? "rawFormat") <*> (x .:? "codingMode") <*>
                     (x .:? "profile")
                     <*> (x .:? "rateControlMode")
                     <*> (x .:? "sampleRate")
                     <*> (x .:? "spec")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "vbrQuality")
                     <*> (x .:? "inputType"))

instance Hashable AacSettings where

instance NFData AacSettings where

instance ToJSON AacSettings where
        toJSON AacSettings'{..}
          = object
              (catMaybes
                 [("rawFormat" .=) <$> _aRawFormat,
                  ("codingMode" .=) <$> _aCodingMode,
                  ("profile" .=) <$> _aProfile,
                  ("rateControlMode" .=) <$> _aRateControlMode,
                  ("sampleRate" .=) <$> _aSampleRate,
                  ("spec" .=) <$> _aSpec, ("bitrate" .=) <$> _aBitrate,
                  ("vbrQuality" .=) <$> _aVbrQuality,
                  ("inputType" .=) <$> _aInputType])

-- | Ac3 Settings
--
-- /See:/ 'ac3Settings' smart constructor.
data Ac3Settings = Ac3Settings'{_asLfeFilter ::
                                !(Maybe Ac3LfeFilter),
                                _asMetadataControl ::
                                !(Maybe Ac3MetadataControl),
                                _asBitstreamMode :: !(Maybe Ac3BitstreamMode),
                                _asCodingMode :: !(Maybe Ac3CodingMode),
                                _asBitrate :: !(Maybe Double),
                                _asDialnorm :: !(Maybe Nat),
                                _asDrcProfile :: !(Maybe Ac3DrcProfile)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Ac3Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asLfeFilter' - When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid in codingMode32Lfe mode.
--
-- * 'asMetadataControl' - When set to "followInput", encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- * 'asBitstreamMode' - Specifies the bitstream mode (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.
--
-- * 'asCodingMode' - Dolby Digital coding mode. Determines number of channels.
--
-- * 'asBitrate' - Average bitrate in bits/second. Valid bitrates depend on the coding mode.
--
-- * 'asDialnorm' - Sets the dialnorm for the output. If excluded and input audio is Dolby Digital, dialnorm will be passed through.
--
-- * 'asDrcProfile' - If set to filmStandard, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
ac3Settings
    :: Ac3Settings
ac3Settings
  = Ac3Settings'{_asLfeFilter = Nothing,
                 _asMetadataControl = Nothing,
                 _asBitstreamMode = Nothing, _asCodingMode = Nothing,
                 _asBitrate = Nothing, _asDialnorm = Nothing,
                 _asDrcProfile = Nothing}

-- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid in codingMode32Lfe mode.
asLfeFilter :: Lens' Ac3Settings (Maybe Ac3LfeFilter)
asLfeFilter = lens _asLfeFilter (\ s a -> s{_asLfeFilter = a})

-- | When set to "followInput", encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
asMetadataControl :: Lens' Ac3Settings (Maybe Ac3MetadataControl)
asMetadataControl = lens _asMetadataControl (\ s a -> s{_asMetadataControl = a})

-- | Specifies the bitstream mode (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.
asBitstreamMode :: Lens' Ac3Settings (Maybe Ac3BitstreamMode)
asBitstreamMode = lens _asBitstreamMode (\ s a -> s{_asBitstreamMode = a})

-- | Dolby Digital coding mode. Determines number of channels.
asCodingMode :: Lens' Ac3Settings (Maybe Ac3CodingMode)
asCodingMode = lens _asCodingMode (\ s a -> s{_asCodingMode = a})

-- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
asBitrate :: Lens' Ac3Settings (Maybe Double)
asBitrate = lens _asBitrate (\ s a -> s{_asBitrate = a})

-- | Sets the dialnorm for the output. If excluded and input audio is Dolby Digital, dialnorm will be passed through.
asDialnorm :: Lens' Ac3Settings (Maybe Natural)
asDialnorm = lens _asDialnorm (\ s a -> s{_asDialnorm = a}) . mapping _Nat

-- | If set to filmStandard, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
asDrcProfile :: Lens' Ac3Settings (Maybe Ac3DrcProfile)
asDrcProfile = lens _asDrcProfile (\ s a -> s{_asDrcProfile = a})

instance FromJSON Ac3Settings where
        parseJSON
          = withObject "Ac3Settings"
              (\ x ->
                 Ac3Settings' <$>
                   (x .:? "lfeFilter") <*> (x .:? "metadataControl") <*>
                     (x .:? "bitstreamMode")
                     <*> (x .:? "codingMode")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "dialnorm")
                     <*> (x .:? "drcProfile"))

instance Hashable Ac3Settings where

instance NFData Ac3Settings where

instance ToJSON Ac3Settings where
        toJSON Ac3Settings'{..}
          = object
              (catMaybes
                 [("lfeFilter" .=) <$> _asLfeFilter,
                  ("metadataControl" .=) <$> _asMetadataControl,
                  ("bitstreamMode" .=) <$> _asBitstreamMode,
                  ("codingMode" .=) <$> _asCodingMode,
                  ("bitrate" .=) <$> _asBitrate,
                  ("dialnorm" .=) <$> _asDialnorm,
                  ("drcProfile" .=) <$> _asDrcProfile])

-- | Archive Container Settings
--
-- /See:/ 'archiveContainerSettings' smart constructor.
newtype ArchiveContainerSettings = ArchiveContainerSettings'{_acsM2tsSettings
                                                             ::
                                                             Maybe M2tsSettings}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'ArchiveContainerSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsM2tsSettings' - Undocumented member.
archiveContainerSettings
    :: ArchiveContainerSettings
archiveContainerSettings
  = ArchiveContainerSettings'{_acsM2tsSettings =
                                Nothing}

-- | Undocumented member.
acsM2tsSettings :: Lens' ArchiveContainerSettings (Maybe M2tsSettings)
acsM2tsSettings = lens _acsM2tsSettings (\ s a -> s{_acsM2tsSettings = a})

instance FromJSON ArchiveContainerSettings where
        parseJSON
          = withObject "ArchiveContainerSettings"
              (\ x ->
                 ArchiveContainerSettings' <$> (x .:? "m2tsSettings"))

instance Hashable ArchiveContainerSettings where

instance NFData ArchiveContainerSettings where

instance ToJSON ArchiveContainerSettings where
        toJSON ArchiveContainerSettings'{..}
          = object
              (catMaybes
                 [("m2tsSettings" .=) <$> _acsM2tsSettings])

-- | Archive Group Settings
--
-- /See:/ 'archiveGroupSettings' smart constructor.
data ArchiveGroupSettings = ArchiveGroupSettings'{_agsRolloverInterval
                                                  :: !(Maybe Nat),
                                                  _agsDestination ::
                                                  !OutputLocationRef}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArchiveGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agsRolloverInterval' - Number of seconds to write to archive file before closing and starting a new one.
--
-- * 'agsDestination' - A directory and base filename where archive files should be written.
archiveGroupSettings
    :: OutputLocationRef -- ^ 'agsDestination'
    -> ArchiveGroupSettings
archiveGroupSettings pDestination_
  = ArchiveGroupSettings'{_agsRolloverInterval =
                            Nothing,
                          _agsDestination = pDestination_}

-- | Number of seconds to write to archive file before closing and starting a new one.
agsRolloverInterval :: Lens' ArchiveGroupSettings (Maybe Natural)
agsRolloverInterval = lens _agsRolloverInterval (\ s a -> s{_agsRolloverInterval = a}) . mapping _Nat

-- | A directory and base filename where archive files should be written.
agsDestination :: Lens' ArchiveGroupSettings OutputLocationRef
agsDestination = lens _agsDestination (\ s a -> s{_agsDestination = a})

instance FromJSON ArchiveGroupSettings where
        parseJSON
          = withObject "ArchiveGroupSettings"
              (\ x ->
                 ArchiveGroupSettings' <$>
                   (x .:? "rolloverInterval") <*> (x .: "destination"))

instance Hashable ArchiveGroupSettings where

instance NFData ArchiveGroupSettings where

instance ToJSON ArchiveGroupSettings where
        toJSON ArchiveGroupSettings'{..}
          = object
              (catMaybes
                 [("rolloverInterval" .=) <$> _agsRolloverInterval,
                  Just ("destination" .= _agsDestination)])

-- | Archive Output Settings
--
-- /See:/ 'archiveOutputSettings' smart constructor.
data ArchiveOutputSettings = ArchiveOutputSettings'{_aosExtension
                                                    :: !(Maybe Text),
                                                    _aosNameModifier ::
                                                    !(Maybe Text),
                                                    _aosContainerSettings ::
                                                    !ArchiveContainerSettings}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'ArchiveOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aosExtension' - Output file extension. If excluded, this will be auto-selected from the container type.
--
-- * 'aosNameModifier' - String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
--
-- * 'aosContainerSettings' - Settings specific to the container type of the file.
archiveOutputSettings
    :: ArchiveContainerSettings -- ^ 'aosContainerSettings'
    -> ArchiveOutputSettings
archiveOutputSettings pContainerSettings_
  = ArchiveOutputSettings'{_aosExtension = Nothing,
                           _aosNameModifier = Nothing,
                           _aosContainerSettings = pContainerSettings_}

-- | Output file extension. If excluded, this will be auto-selected from the container type.
aosExtension :: Lens' ArchiveOutputSettings (Maybe Text)
aosExtension = lens _aosExtension (\ s a -> s{_aosExtension = a})

-- | String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
aosNameModifier :: Lens' ArchiveOutputSettings (Maybe Text)
aosNameModifier = lens _aosNameModifier (\ s a -> s{_aosNameModifier = a})

-- | Settings specific to the container type of the file.
aosContainerSettings :: Lens' ArchiveOutputSettings ArchiveContainerSettings
aosContainerSettings = lens _aosContainerSettings (\ s a -> s{_aosContainerSettings = a})

instance FromJSON ArchiveOutputSettings where
        parseJSON
          = withObject "ArchiveOutputSettings"
              (\ x ->
                 ArchiveOutputSettings' <$>
                   (x .:? "extension") <*> (x .:? "nameModifier") <*>
                     (x .: "containerSettings"))

instance Hashable ArchiveOutputSettings where

instance NFData ArchiveOutputSettings where

instance ToJSON ArchiveOutputSettings where
        toJSON ArchiveOutputSettings'{..}
          = object
              (catMaybes
                 [("extension" .=) <$> _aosExtension,
                  ("nameModifier" .=) <$> _aosNameModifier,
                  Just ("containerSettings" .= _aosContainerSettings)])

-- | Arib Destination Settings
--
-- /See:/ 'aribDestinationSettings' smart constructor.
data AribDestinationSettings = AribDestinationSettings'
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'AribDestinationSettings' with the minimum fields required to make a request.
--
aribDestinationSettings
    :: AribDestinationSettings
aribDestinationSettings = AribDestinationSettings'

instance FromJSON AribDestinationSettings where
        parseJSON
          = withObject "AribDestinationSettings"
              (\ x -> pure AribDestinationSettings')

instance Hashable AribDestinationSettings where

instance NFData AribDestinationSettings where

instance ToJSON AribDestinationSettings where
        toJSON = const (Object mempty)

-- | Arib Source Settings
--
-- /See:/ 'aribSourceSettings' smart constructor.
data AribSourceSettings = AribSourceSettings'
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AribSourceSettings' with the minimum fields required to make a request.
--
aribSourceSettings
    :: AribSourceSettings
aribSourceSettings = AribSourceSettings'

instance FromJSON AribSourceSettings where
        parseJSON
          = withObject "AribSourceSettings"
              (\ x -> pure AribSourceSettings')

instance Hashable AribSourceSettings where

instance NFData AribSourceSettings where

instance ToJSON AribSourceSettings where
        toJSON = const (Object mempty)

-- | Audio Channel Mapping
--
-- /See:/ 'audioChannelMapping' smart constructor.
data AudioChannelMapping = AudioChannelMapping'{_acmOutputChannel
                                                :: !Nat,
                                                _acmInputChannelLevels ::
                                                ![InputChannelLevel]}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioChannelMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acmOutputChannel' - The index of the output channel being produced.
--
-- * 'acmInputChannelLevels' - Indices and gain values for each input channel that should be remixed into this output channel.
audioChannelMapping
    :: Natural -- ^ 'acmOutputChannel'
    -> AudioChannelMapping
audioChannelMapping pOutputChannel_
  = AudioChannelMapping'{_acmOutputChannel =
                           _Nat # pOutputChannel_,
                         _acmInputChannelLevels = mempty}

-- | The index of the output channel being produced.
acmOutputChannel :: Lens' AudioChannelMapping Natural
acmOutputChannel = lens _acmOutputChannel (\ s a -> s{_acmOutputChannel = a}) . _Nat

-- | Indices and gain values for each input channel that should be remixed into this output channel.
acmInputChannelLevels :: Lens' AudioChannelMapping [InputChannelLevel]
acmInputChannelLevels = lens _acmInputChannelLevels (\ s a -> s{_acmInputChannelLevels = a}) . _Coerce

instance FromJSON AudioChannelMapping where
        parseJSON
          = withObject "AudioChannelMapping"
              (\ x ->
                 AudioChannelMapping' <$>
                   (x .: "outputChannel") <*>
                     (x .:? "inputChannelLevels" .!= mempty))

instance Hashable AudioChannelMapping where

instance NFData AudioChannelMapping where

instance ToJSON AudioChannelMapping where
        toJSON AudioChannelMapping'{..}
          = object
              (catMaybes
                 [Just ("outputChannel" .= _acmOutputChannel),
                  Just
                    ("inputChannelLevels" .= _acmInputChannelLevels)])

-- | Audio Codec Settings
--
-- /See:/ 'audioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'{_acsPassThroughSettings
                                              :: !(Maybe PassThroughSettings),
                                              _acsAc3Settings ::
                                              !(Maybe Ac3Settings),
                                              _acsMp2Settings ::
                                              !(Maybe Mp2Settings),
                                              _acsAacSettings ::
                                              !(Maybe AacSettings),
                                              _acsEac3Settings ::
                                              !(Maybe Eac3Settings)}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioCodecSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsPassThroughSettings' - Undocumented member.
--
-- * 'acsAc3Settings' - Undocumented member.
--
-- * 'acsMp2Settings' - Undocumented member.
--
-- * 'acsAacSettings' - Undocumented member.
--
-- * 'acsEac3Settings' - Undocumented member.
audioCodecSettings
    :: AudioCodecSettings
audioCodecSettings
  = AudioCodecSettings'{_acsPassThroughSettings =
                          Nothing,
                        _acsAc3Settings = Nothing, _acsMp2Settings = Nothing,
                        _acsAacSettings = Nothing,
                        _acsEac3Settings = Nothing}

-- | Undocumented member.
acsPassThroughSettings :: Lens' AudioCodecSettings (Maybe PassThroughSettings)
acsPassThroughSettings = lens _acsPassThroughSettings (\ s a -> s{_acsPassThroughSettings = a})

-- | Undocumented member.
acsAc3Settings :: Lens' AudioCodecSettings (Maybe Ac3Settings)
acsAc3Settings = lens _acsAc3Settings (\ s a -> s{_acsAc3Settings = a})

-- | Undocumented member.
acsMp2Settings :: Lens' AudioCodecSettings (Maybe Mp2Settings)
acsMp2Settings = lens _acsMp2Settings (\ s a -> s{_acsMp2Settings = a})

-- | Undocumented member.
acsAacSettings :: Lens' AudioCodecSettings (Maybe AacSettings)
acsAacSettings = lens _acsAacSettings (\ s a -> s{_acsAacSettings = a})

-- | Undocumented member.
acsEac3Settings :: Lens' AudioCodecSettings (Maybe Eac3Settings)
acsEac3Settings = lens _acsEac3Settings (\ s a -> s{_acsEac3Settings = a})

instance FromJSON AudioCodecSettings where
        parseJSON
          = withObject "AudioCodecSettings"
              (\ x ->
                 AudioCodecSettings' <$>
                   (x .:? "passThroughSettings") <*>
                     (x .:? "ac3Settings")
                     <*> (x .:? "mp2Settings")
                     <*> (x .:? "aacSettings")
                     <*> (x .:? "eac3Settings"))

instance Hashable AudioCodecSettings where

instance NFData AudioCodecSettings where

instance ToJSON AudioCodecSettings where
        toJSON AudioCodecSettings'{..}
          = object
              (catMaybes
                 [("passThroughSettings" .=) <$>
                    _acsPassThroughSettings,
                  ("ac3Settings" .=) <$> _acsAc3Settings,
                  ("mp2Settings" .=) <$> _acsMp2Settings,
                  ("aacSettings" .=) <$> _acsAacSettings,
                  ("eac3Settings" .=) <$> _acsEac3Settings])

-- | Audio Description
--
-- /See:/ 'audioDescription' smart constructor.
data AudioDescription = AudioDescription'{_adLanguageCode
                                          :: !(Maybe Text),
                                          _adAudioType :: !(Maybe AudioType),
                                          _adAudioNormalizationSettings ::
                                          !(Maybe AudioNormalizationSettings),
                                          _adLanguageCodeControl ::
                                          !(Maybe
                                              AudioDescriptionLanguageCodeControl),
                                          _adCodecSettings ::
                                          !(Maybe AudioCodecSettings),
                                          _adStreamName :: !(Maybe Text),
                                          _adRemixSettings ::
                                          !(Maybe RemixSettings),
                                          _adAudioTypeControl ::
                                          !(Maybe
                                              AudioDescriptionAudioTypeControl),
                                          _adAudioSelectorName :: !Text,
                                          _adName :: !Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adLanguageCode' - Indicates the language of the audio output track. Only used if languageControlMode is useConfigured, or there is no ISO 639 language code specified in the input.
--
-- * 'adAudioType' - Applies only if audioTypeControl is useConfigured. The values for audioType are defined in ISO-IEC 13818-1.
--
-- * 'adAudioNormalizationSettings' - Advanced audio normalization settings.
--
-- * 'adLanguageCodeControl' - Choosing followInput will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The languageCode will be used when useConfigured is set, or when followInput is selected but there is no ISO 639 language code specified by the input.
--
-- * 'adCodecSettings' - Audio codec settings.
--
-- * 'adStreamName' - Used for MS Smooth and Apple HLS outputs. Indicates the name displayed by the player (eg. English, or Director Commentary).
--
-- * 'adRemixSettings' - Settings that control how input audio channels are remixed into the output audio channels.
--
-- * 'adAudioTypeControl' - Determines how audio type is determined.   followInput: If the input contains an ISO 639 audioType, then that value is passed through to the output. If the input contains no ISO 639 audioType, the value in Audio Type is included in the output.   useConfigured: The value in Audio Type is included in the output. Note that this field and audioType are both ignored if inputType is broadcasterMixedAd.
--
-- * 'adAudioSelectorName' - The name of the AudioSelector used as the source for this AudioDescription.
--
-- * 'adName' - The name of this AudioDescription. Outputs will use this name to uniquely identify this AudioDescription.  Description names should be unique within this Live Event.
audioDescription
    :: Text -- ^ 'adAudioSelectorName'
    -> Text -- ^ 'adName'
    -> AudioDescription
audioDescription pAudioSelectorName_ pName_
  = AudioDescription'{_adLanguageCode = Nothing,
                      _adAudioType = Nothing,
                      _adAudioNormalizationSettings = Nothing,
                      _adLanguageCodeControl = Nothing,
                      _adCodecSettings = Nothing, _adStreamName = Nothing,
                      _adRemixSettings = Nothing,
                      _adAudioTypeControl = Nothing,
                      _adAudioSelectorName = pAudioSelectorName_,
                      _adName = pName_}

-- | Indicates the language of the audio output track. Only used if languageControlMode is useConfigured, or there is no ISO 639 language code specified in the input.
adLanguageCode :: Lens' AudioDescription (Maybe Text)
adLanguageCode = lens _adLanguageCode (\ s a -> s{_adLanguageCode = a})

-- | Applies only if audioTypeControl is useConfigured. The values for audioType are defined in ISO-IEC 13818-1.
adAudioType :: Lens' AudioDescription (Maybe AudioType)
adAudioType = lens _adAudioType (\ s a -> s{_adAudioType = a})

-- | Advanced audio normalization settings.
adAudioNormalizationSettings :: Lens' AudioDescription (Maybe AudioNormalizationSettings)
adAudioNormalizationSettings = lens _adAudioNormalizationSettings (\ s a -> s{_adAudioNormalizationSettings = a})

-- | Choosing followInput will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The languageCode will be used when useConfigured is set, or when followInput is selected but there is no ISO 639 language code specified by the input.
adLanguageCodeControl :: Lens' AudioDescription (Maybe AudioDescriptionLanguageCodeControl)
adLanguageCodeControl = lens _adLanguageCodeControl (\ s a -> s{_adLanguageCodeControl = a})

-- | Audio codec settings.
adCodecSettings :: Lens' AudioDescription (Maybe AudioCodecSettings)
adCodecSettings = lens _adCodecSettings (\ s a -> s{_adCodecSettings = a})

-- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed by the player (eg. English, or Director Commentary).
adStreamName :: Lens' AudioDescription (Maybe Text)
adStreamName = lens _adStreamName (\ s a -> s{_adStreamName = a})

-- | Settings that control how input audio channels are remixed into the output audio channels.
adRemixSettings :: Lens' AudioDescription (Maybe RemixSettings)
adRemixSettings = lens _adRemixSettings (\ s a -> s{_adRemixSettings = a})

-- | Determines how audio type is determined.   followInput: If the input contains an ISO 639 audioType, then that value is passed through to the output. If the input contains no ISO 639 audioType, the value in Audio Type is included in the output.   useConfigured: The value in Audio Type is included in the output. Note that this field and audioType are both ignored if inputType is broadcasterMixedAd.
adAudioTypeControl :: Lens' AudioDescription (Maybe AudioDescriptionAudioTypeControl)
adAudioTypeControl = lens _adAudioTypeControl (\ s a -> s{_adAudioTypeControl = a})

-- | The name of the AudioSelector used as the source for this AudioDescription.
adAudioSelectorName :: Lens' AudioDescription Text
adAudioSelectorName = lens _adAudioSelectorName (\ s a -> s{_adAudioSelectorName = a})

-- | The name of this AudioDescription. Outputs will use this name to uniquely identify this AudioDescription.  Description names should be unique within this Live Event.
adName :: Lens' AudioDescription Text
adName = lens _adName (\ s a -> s{_adName = a})

instance FromJSON AudioDescription where
        parseJSON
          = withObject "AudioDescription"
              (\ x ->
                 AudioDescription' <$>
                   (x .:? "languageCode") <*> (x .:? "audioType") <*>
                     (x .:? "audioNormalizationSettings")
                     <*> (x .:? "languageCodeControl")
                     <*> (x .:? "codecSettings")
                     <*> (x .:? "streamName")
                     <*> (x .:? "remixSettings")
                     <*> (x .:? "audioTypeControl")
                     <*> (x .: "audioSelectorName")
                     <*> (x .: "name"))

instance Hashable AudioDescription where

instance NFData AudioDescription where

instance ToJSON AudioDescription where
        toJSON AudioDescription'{..}
          = object
              (catMaybes
                 [("languageCode" .=) <$> _adLanguageCode,
                  ("audioType" .=) <$> _adAudioType,
                  ("audioNormalizationSettings" .=) <$>
                    _adAudioNormalizationSettings,
                  ("languageCodeControl" .=) <$>
                    _adLanguageCodeControl,
                  ("codecSettings" .=) <$> _adCodecSettings,
                  ("streamName" .=) <$> _adStreamName,
                  ("remixSettings" .=) <$> _adRemixSettings,
                  ("audioTypeControl" .=) <$> _adAudioTypeControl,
                  Just ("audioSelectorName" .= _adAudioSelectorName),
                  Just ("name" .= _adName)])

-- | Audio Language Selection
--
-- /See:/ 'audioLanguageSelection' smart constructor.
data AudioLanguageSelection = AudioLanguageSelection'{_alsLanguageSelectionPolicy
                                                      ::
                                                      !(Maybe
                                                          AudioLanguageSelectionPolicy),
                                                      _alsLanguageCode :: !Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'AudioLanguageSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alsLanguageSelectionPolicy' - When set to "strict", the transport stream demux strictly identifies audio streams by their language descriptor. If a PMT update occurs such that an audio stream matching the initially selected language is no longer present then mute will be encoded until the language returns. If "loose", then on a PMT update the demux will choose another audio stream in the program with the same stream type if it can't find one with the same language.
--
-- * 'alsLanguageCode' - Selects a specific three-letter language code from within an audio source.
audioLanguageSelection
    :: Text -- ^ 'alsLanguageCode'
    -> AudioLanguageSelection
audioLanguageSelection pLanguageCode_
  = AudioLanguageSelection'{_alsLanguageSelectionPolicy
                              = Nothing,
                            _alsLanguageCode = pLanguageCode_}

-- | When set to "strict", the transport stream demux strictly identifies audio streams by their language descriptor. If a PMT update occurs such that an audio stream matching the initially selected language is no longer present then mute will be encoded until the language returns. If "loose", then on a PMT update the demux will choose another audio stream in the program with the same stream type if it can't find one with the same language.
alsLanguageSelectionPolicy :: Lens' AudioLanguageSelection (Maybe AudioLanguageSelectionPolicy)
alsLanguageSelectionPolicy = lens _alsLanguageSelectionPolicy (\ s a -> s{_alsLanguageSelectionPolicy = a})

-- | Selects a specific three-letter language code from within an audio source.
alsLanguageCode :: Lens' AudioLanguageSelection Text
alsLanguageCode = lens _alsLanguageCode (\ s a -> s{_alsLanguageCode = a})

instance FromJSON AudioLanguageSelection where
        parseJSON
          = withObject "AudioLanguageSelection"
              (\ x ->
                 AudioLanguageSelection' <$>
                   (x .:? "languageSelectionPolicy") <*>
                     (x .: "languageCode"))

instance Hashable AudioLanguageSelection where

instance NFData AudioLanguageSelection where

instance ToJSON AudioLanguageSelection where
        toJSON AudioLanguageSelection'{..}
          = object
              (catMaybes
                 [("languageSelectionPolicy" .=) <$>
                    _alsLanguageSelectionPolicy,
                  Just ("languageCode" .= _alsLanguageCode)])

-- | Audio Normalization Settings
--
-- /See:/ 'audioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'{_ansAlgorithmControl
                                                              ::
                                                              !(Maybe
                                                                  AudioNormalizationAlgorithmControl),
                                                              _ansTargetLkfs ::
                                                              !(Maybe Double),
                                                              _ansAlgorithm ::
                                                              !(Maybe
                                                                  AudioNormalizationAlgorithm)}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'AudioNormalizationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ansAlgorithmControl' - When set to correctAudio the output audio is corrected using the chosen algorithm. If set to measureOnly, the audio will be measured but not adjusted.
--
-- * 'ansTargetLkfs' - Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm.  The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
--
-- * 'ansAlgorithm' - Audio normalization algorithm to use. itu17701 conforms to the CALM Act specification, itu17702 conforms to the EBU R-128 specification.
audioNormalizationSettings
    :: AudioNormalizationSettings
audioNormalizationSettings
  = AudioNormalizationSettings'{_ansAlgorithmControl =
                                  Nothing,
                                _ansTargetLkfs = Nothing,
                                _ansAlgorithm = Nothing}

-- | When set to correctAudio the output audio is corrected using the chosen algorithm. If set to measureOnly, the audio will be measured but not adjusted.
ansAlgorithmControl :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationAlgorithmControl)
ansAlgorithmControl = lens _ansAlgorithmControl (\ s a -> s{_ansAlgorithmControl = a})

-- | Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm.  The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
ansTargetLkfs :: Lens' AudioNormalizationSettings (Maybe Double)
ansTargetLkfs = lens _ansTargetLkfs (\ s a -> s{_ansTargetLkfs = a})

-- | Audio normalization algorithm to use. itu17701 conforms to the CALM Act specification, itu17702 conforms to the EBU R-128 specification.
ansAlgorithm :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationAlgorithm)
ansAlgorithm = lens _ansAlgorithm (\ s a -> s{_ansAlgorithm = a})

instance FromJSON AudioNormalizationSettings where
        parseJSON
          = withObject "AudioNormalizationSettings"
              (\ x ->
                 AudioNormalizationSettings' <$>
                   (x .:? "algorithmControl") <*> (x .:? "targetLkfs")
                     <*> (x .:? "algorithm"))

instance Hashable AudioNormalizationSettings where

instance NFData AudioNormalizationSettings where

instance ToJSON AudioNormalizationSettings where
        toJSON AudioNormalizationSettings'{..}
          = object
              (catMaybes
                 [("algorithmControl" .=) <$> _ansAlgorithmControl,
                  ("targetLkfs" .=) <$> _ansTargetLkfs,
                  ("algorithm" .=) <$> _ansAlgorithm])

-- | Audio Only Hls Settings
--
-- /See:/ 'audioOnlyHlsSettings' smart constructor.
data AudioOnlyHlsSettings = AudioOnlyHlsSettings'{_aohsAudioOnlyImage
                                                  :: !(Maybe InputLocation),
                                                  _aohsSegmentType ::
                                                  !(Maybe
                                                      AudioOnlyHlsSegmentType),
                                                  _aohsAudioGroupId ::
                                                  !(Maybe Text),
                                                  _aohsAudioTrackType ::
                                                  !(Maybe
                                                      AudioOnlyHlsTrackType)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioOnlyHlsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aohsAudioOnlyImage' - Optional. Specifies the .jpg or .png image to use as the cover art for an audio-only output. We recommend a low bit-size file because the image increases the output audio bandwidth. The image is attached to the audio as an ID3 tag, frame type APIC, picture type 0x10, as per the "ID3 tag version 2.4.0 - Native Frames" standard.
--
-- * 'aohsSegmentType' - Specifies the segment type.
--
-- * 'aohsAudioGroupId' - Specifies the group to which the audio Rendition belongs.
--
-- * 'aohsAudioTrackType' - Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
audioOnlyHlsSettings
    :: AudioOnlyHlsSettings
audioOnlyHlsSettings
  = AudioOnlyHlsSettings'{_aohsAudioOnlyImage =
                            Nothing,
                          _aohsSegmentType = Nothing,
                          _aohsAudioGroupId = Nothing,
                          _aohsAudioTrackType = Nothing}

-- | Optional. Specifies the .jpg or .png image to use as the cover art for an audio-only output. We recommend a low bit-size file because the image increases the output audio bandwidth. The image is attached to the audio as an ID3 tag, frame type APIC, picture type 0x10, as per the "ID3 tag version 2.4.0 - Native Frames" standard.
aohsAudioOnlyImage :: Lens' AudioOnlyHlsSettings (Maybe InputLocation)
aohsAudioOnlyImage = lens _aohsAudioOnlyImage (\ s a -> s{_aohsAudioOnlyImage = a})

-- | Specifies the segment type.
aohsSegmentType :: Lens' AudioOnlyHlsSettings (Maybe AudioOnlyHlsSegmentType)
aohsSegmentType = lens _aohsSegmentType (\ s a -> s{_aohsSegmentType = a})

-- | Specifies the group to which the audio Rendition belongs.
aohsAudioGroupId :: Lens' AudioOnlyHlsSettings (Maybe Text)
aohsAudioGroupId = lens _aohsAudioGroupId (\ s a -> s{_aohsAudioGroupId = a})

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
aohsAudioTrackType :: Lens' AudioOnlyHlsSettings (Maybe AudioOnlyHlsTrackType)
aohsAudioTrackType = lens _aohsAudioTrackType (\ s a -> s{_aohsAudioTrackType = a})

instance FromJSON AudioOnlyHlsSettings where
        parseJSON
          = withObject "AudioOnlyHlsSettings"
              (\ x ->
                 AudioOnlyHlsSettings' <$>
                   (x .:? "audioOnlyImage") <*> (x .:? "segmentType")
                     <*> (x .:? "audioGroupId")
                     <*> (x .:? "audioTrackType"))

instance Hashable AudioOnlyHlsSettings where

instance NFData AudioOnlyHlsSettings where

instance ToJSON AudioOnlyHlsSettings where
        toJSON AudioOnlyHlsSettings'{..}
          = object
              (catMaybes
                 [("audioOnlyImage" .=) <$> _aohsAudioOnlyImage,
                  ("segmentType" .=) <$> _aohsSegmentType,
                  ("audioGroupId" .=) <$> _aohsAudioGroupId,
                  ("audioTrackType" .=) <$> _aohsAudioTrackType])

-- | Audio Pid Selection
--
-- /See:/ 'audioPidSelection' smart constructor.
newtype AudioPidSelection = AudioPidSelection'{_apsPid
                                               :: Nat}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioPidSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apsPid' - Selects a specific PID from within a source.
audioPidSelection
    :: Natural -- ^ 'apsPid'
    -> AudioPidSelection
audioPidSelection pPid_
  = AudioPidSelection'{_apsPid = _Nat # pPid_}

-- | Selects a specific PID from within a source.
apsPid :: Lens' AudioPidSelection Natural
apsPid = lens _apsPid (\ s a -> s{_apsPid = a}) . _Nat

instance FromJSON AudioPidSelection where
        parseJSON
          = withObject "AudioPidSelection"
              (\ x -> AudioPidSelection' <$> (x .: "pid"))

instance Hashable AudioPidSelection where

instance NFData AudioPidSelection where

instance ToJSON AudioPidSelection where
        toJSON AudioPidSelection'{..}
          = object (catMaybes [Just ("pid" .= _apsPid)])

-- | Audio Selector
--
-- /See:/ 'audioSelector' smart constructor.
data AudioSelector = AudioSelector'{_asSelectorSettings
                                    :: !(Maybe AudioSelectorSettings),
                                    _asName :: !Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asSelectorSettings' - The audio selector settings.
--
-- * 'asName' - The name of this AudioSelector. AudioDescriptions will use this name to uniquely identify this Selector.  Selector names should be unique per input.
audioSelector
    :: Text -- ^ 'asName'
    -> AudioSelector
audioSelector pName_
  = AudioSelector'{_asSelectorSettings = Nothing,
                   _asName = pName_}

-- | The audio selector settings.
asSelectorSettings :: Lens' AudioSelector (Maybe AudioSelectorSettings)
asSelectorSettings = lens _asSelectorSettings (\ s a -> s{_asSelectorSettings = a})

-- | The name of this AudioSelector. AudioDescriptions will use this name to uniquely identify this Selector.  Selector names should be unique per input.
asName :: Lens' AudioSelector Text
asName = lens _asName (\ s a -> s{_asName = a})

instance FromJSON AudioSelector where
        parseJSON
          = withObject "AudioSelector"
              (\ x ->
                 AudioSelector' <$>
                   (x .:? "selectorSettings") <*> (x .: "name"))

instance Hashable AudioSelector where

instance NFData AudioSelector where

instance ToJSON AudioSelector where
        toJSON AudioSelector'{..}
          = object
              (catMaybes
                 [("selectorSettings" .=) <$> _asSelectorSettings,
                  Just ("name" .= _asName)])

-- | Audio Selector Settings
--
-- /See:/ 'audioSelectorSettings' smart constructor.
data AudioSelectorSettings = AudioSelectorSettings'{_assAudioLanguageSelection
                                                    ::
                                                    !(Maybe
                                                        AudioLanguageSelection),
                                                    _assAudioTrackSelection ::
                                                    !(Maybe
                                                        AudioTrackSelection),
                                                    _assAudioPidSelection ::
                                                    !(Maybe AudioPidSelection)}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'AudioSelectorSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assAudioLanguageSelection' - Undocumented member.
--
-- * 'assAudioTrackSelection' - Undocumented member.
--
-- * 'assAudioPidSelection' - Undocumented member.
audioSelectorSettings
    :: AudioSelectorSettings
audioSelectorSettings
  = AudioSelectorSettings'{_assAudioLanguageSelection =
                             Nothing,
                           _assAudioTrackSelection = Nothing,
                           _assAudioPidSelection = Nothing}

-- | Undocumented member.
assAudioLanguageSelection :: Lens' AudioSelectorSettings (Maybe AudioLanguageSelection)
assAudioLanguageSelection = lens _assAudioLanguageSelection (\ s a -> s{_assAudioLanguageSelection = a})

-- | Undocumented member.
assAudioTrackSelection :: Lens' AudioSelectorSettings (Maybe AudioTrackSelection)
assAudioTrackSelection = lens _assAudioTrackSelection (\ s a -> s{_assAudioTrackSelection = a})

-- | Undocumented member.
assAudioPidSelection :: Lens' AudioSelectorSettings (Maybe AudioPidSelection)
assAudioPidSelection = lens _assAudioPidSelection (\ s a -> s{_assAudioPidSelection = a})

instance FromJSON AudioSelectorSettings where
        parseJSON
          = withObject "AudioSelectorSettings"
              (\ x ->
                 AudioSelectorSettings' <$>
                   (x .:? "audioLanguageSelection") <*>
                     (x .:? "audioTrackSelection")
                     <*> (x .:? "audioPidSelection"))

instance Hashable AudioSelectorSettings where

instance NFData AudioSelectorSettings where

instance ToJSON AudioSelectorSettings where
        toJSON AudioSelectorSettings'{..}
          = object
              (catMaybes
                 [("audioLanguageSelection" .=) <$>
                    _assAudioLanguageSelection,
                  ("audioTrackSelection" .=) <$>
                    _assAudioTrackSelection,
                  ("audioPidSelection" .=) <$> _assAudioPidSelection])

-- | Audio Track
--
-- /See:/ 'audioTrack' smart constructor.
newtype AudioTrack = AudioTrack'{_atTrack :: Nat}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioTrack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atTrack' - 1-based integer value that maps to a specific audio track
audioTrack
    :: Natural -- ^ 'atTrack'
    -> AudioTrack
audioTrack pTrack_
  = AudioTrack'{_atTrack = _Nat # pTrack_}

-- | 1-based integer value that maps to a specific audio track
atTrack :: Lens' AudioTrack Natural
atTrack = lens _atTrack (\ s a -> s{_atTrack = a}) . _Nat

instance FromJSON AudioTrack where
        parseJSON
          = withObject "AudioTrack"
              (\ x -> AudioTrack' <$> (x .: "track"))

instance Hashable AudioTrack where

instance NFData AudioTrack where

instance ToJSON AudioTrack where
        toJSON AudioTrack'{..}
          = object (catMaybes [Just ("track" .= _atTrack)])

-- | Audio Track Selection
--
-- /See:/ 'audioTrackSelection' smart constructor.
newtype AudioTrackSelection = AudioTrackSelection'{_atsTracks
                                                   :: [AudioTrack]}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'AudioTrackSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atsTracks' - Selects one or more unique audio tracks from within an mp4 source.
audioTrackSelection
    :: AudioTrackSelection
audioTrackSelection
  = AudioTrackSelection'{_atsTracks = mempty}

-- | Selects one or more unique audio tracks from within an mp4 source.
atsTracks :: Lens' AudioTrackSelection [AudioTrack]
atsTracks = lens _atsTracks (\ s a -> s{_atsTracks = a}) . _Coerce

instance FromJSON AudioTrackSelection where
        parseJSON
          = withObject "AudioTrackSelection"
              (\ x ->
                 AudioTrackSelection' <$> (x .:? "tracks" .!= mempty))

instance Hashable AudioTrackSelection where

instance NFData AudioTrackSelection where

instance ToJSON AudioTrackSelection where
        toJSON AudioTrackSelection'{..}
          = object (catMaybes [Just ("tracks" .= _atsTracks)])

-- | The settings for Automatic Input Failover.
--
-- /See:/ 'automaticInputFailoverSettings' smart constructor.
data AutomaticInputFailoverSettings = AutomaticInputFailoverSettings'{_aifsInputPreference
                                                                      ::
                                                                      !(Maybe
                                                                          InputPreference),
                                                                      _aifsSecondaryInputId
                                                                      :: !Text}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'AutomaticInputFailoverSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aifsInputPreference' - Input preference when deciding which input to make active when a previously failed input has recovered.
--
-- * 'aifsSecondaryInputId' - The input ID of the secondary input in the automatic input failover pair.
automaticInputFailoverSettings
    :: Text -- ^ 'aifsSecondaryInputId'
    -> AutomaticInputFailoverSettings
automaticInputFailoverSettings pSecondaryInputId_
  = AutomaticInputFailoverSettings'{_aifsInputPreference
                                      = Nothing,
                                    _aifsSecondaryInputId = pSecondaryInputId_}

-- | Input preference when deciding which input to make active when a previously failed input has recovered.
aifsInputPreference :: Lens' AutomaticInputFailoverSettings (Maybe InputPreference)
aifsInputPreference = lens _aifsInputPreference (\ s a -> s{_aifsInputPreference = a})

-- | The input ID of the secondary input in the automatic input failover pair.
aifsSecondaryInputId :: Lens' AutomaticInputFailoverSettings Text
aifsSecondaryInputId = lens _aifsSecondaryInputId (\ s a -> s{_aifsSecondaryInputId = a})

instance FromJSON AutomaticInputFailoverSettings
         where
        parseJSON
          = withObject "AutomaticInputFailoverSettings"
              (\ x ->
                 AutomaticInputFailoverSettings' <$>
                   (x .:? "inputPreference") <*>
                     (x .: "secondaryInputId"))

instance Hashable AutomaticInputFailoverSettings
         where

instance NFData AutomaticInputFailoverSettings where

instance ToJSON AutomaticInputFailoverSettings where
        toJSON AutomaticInputFailoverSettings'{..}
          = object
              (catMaybes
                 [("inputPreference" .=) <$> _aifsInputPreference,
                  Just ("secondaryInputId" .= _aifsSecondaryInputId)])

-- | Avail Blanking
--
-- /See:/ 'availBlanking' smart constructor.
data AvailBlanking = AvailBlanking'{_abState ::
                                    !(Maybe AvailBlankingState),
                                    _abAvailBlankingImage ::
                                    !(Maybe InputLocation)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailBlanking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abState' - When set to enabled, causes video, audio and captions to be blanked when insertion metadata is added.
--
-- * 'abAvailBlankingImage' - Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
availBlanking
    :: AvailBlanking
availBlanking
  = AvailBlanking'{_abState = Nothing,
                   _abAvailBlankingImage = Nothing}

-- | When set to enabled, causes video, audio and captions to be blanked when insertion metadata is added.
abState :: Lens' AvailBlanking (Maybe AvailBlankingState)
abState = lens _abState (\ s a -> s{_abState = a})

-- | Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
abAvailBlankingImage :: Lens' AvailBlanking (Maybe InputLocation)
abAvailBlankingImage = lens _abAvailBlankingImage (\ s a -> s{_abAvailBlankingImage = a})

instance FromJSON AvailBlanking where
        parseJSON
          = withObject "AvailBlanking"
              (\ x ->
                 AvailBlanking' <$>
                   (x .:? "state") <*> (x .:? "availBlankingImage"))

instance Hashable AvailBlanking where

instance NFData AvailBlanking where

instance ToJSON AvailBlanking where
        toJSON AvailBlanking'{..}
          = object
              (catMaybes
                 [("state" .=) <$> _abState,
                  ("availBlankingImage" .=) <$> _abAvailBlankingImage])

-- | Avail Configuration
--
-- /See:/ 'availConfiguration' smart constructor.
newtype AvailConfiguration = AvailConfiguration'{_acAvailSettings
                                                 :: Maybe AvailSettings}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'AvailConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acAvailSettings' - Ad avail settings.
availConfiguration
    :: AvailConfiguration
availConfiguration
  = AvailConfiguration'{_acAvailSettings = Nothing}

-- | Ad avail settings.
acAvailSettings :: Lens' AvailConfiguration (Maybe AvailSettings)
acAvailSettings = lens _acAvailSettings (\ s a -> s{_acAvailSettings = a})

instance FromJSON AvailConfiguration where
        parseJSON
          = withObject "AvailConfiguration"
              (\ x ->
                 AvailConfiguration' <$> (x .:? "availSettings"))

instance Hashable AvailConfiguration where

instance NFData AvailConfiguration where

instance ToJSON AvailConfiguration where
        toJSON AvailConfiguration'{..}
          = object
              (catMaybes
                 [("availSettings" .=) <$> _acAvailSettings])

-- | Avail Settings
--
-- /See:/ 'availSettings' smart constructor.
data AvailSettings = AvailSettings'{_asScte35SpliceInsert
                                    :: !(Maybe Scte35SpliceInsert),
                                    _asScte35TimeSignalApos ::
                                    !(Maybe Scte35TimeSignalApos)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asScte35SpliceInsert' - Undocumented member.
--
-- * 'asScte35TimeSignalApos' - Undocumented member.
availSettings
    :: AvailSettings
availSettings
  = AvailSettings'{_asScte35SpliceInsert = Nothing,
                   _asScte35TimeSignalApos = Nothing}

-- | Undocumented member.
asScte35SpliceInsert :: Lens' AvailSettings (Maybe Scte35SpliceInsert)
asScte35SpliceInsert = lens _asScte35SpliceInsert (\ s a -> s{_asScte35SpliceInsert = a})

-- | Undocumented member.
asScte35TimeSignalApos :: Lens' AvailSettings (Maybe Scte35TimeSignalApos)
asScte35TimeSignalApos = lens _asScte35TimeSignalApos (\ s a -> s{_asScte35TimeSignalApos = a})

instance FromJSON AvailSettings where
        parseJSON
          = withObject "AvailSettings"
              (\ x ->
                 AvailSettings' <$>
                   (x .:? "scte35SpliceInsert") <*>
                     (x .:? "scte35TimeSignalApos"))

instance Hashable AvailSettings where

instance NFData AvailSettings where

instance ToJSON AvailSettings where
        toJSON AvailSettings'{..}
          = object
              (catMaybes
                 [("scte35SpliceInsert" .=) <$> _asScte35SpliceInsert,
                  ("scte35TimeSignalApos" .=) <$>
                    _asScte35TimeSignalApos])

-- | A list of schedule actions to create (in a request) or that have been created (in a response).
--
-- /See:/ 'batchScheduleActionCreateRequest' smart constructor.
newtype BatchScheduleActionCreateRequest = BatchScheduleActionCreateRequest'{_bsacrScheduleActions
                                                                             ::
                                                                             [ScheduleAction]}
                                             deriving (Eq, Read, Show, Data,
                                                       Typeable, Generic)

-- | Creates a value of 'BatchScheduleActionCreateRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsacrScheduleActions' - A list of schedule actions to create.
batchScheduleActionCreateRequest
    :: BatchScheduleActionCreateRequest
batchScheduleActionCreateRequest
  = BatchScheduleActionCreateRequest'{_bsacrScheduleActions
                                        = mempty}

-- | A list of schedule actions to create.
bsacrScheduleActions :: Lens' BatchScheduleActionCreateRequest [ScheduleAction]
bsacrScheduleActions = lens _bsacrScheduleActions (\ s a -> s{_bsacrScheduleActions = a}) . _Coerce

instance Hashable BatchScheduleActionCreateRequest
         where

instance NFData BatchScheduleActionCreateRequest
         where

instance ToJSON BatchScheduleActionCreateRequest
         where
        toJSON BatchScheduleActionCreateRequest'{..}
          = object
              (catMaybes
                 [Just ("scheduleActions" .= _bsacrScheduleActions)])

-- | List of actions that have been created in the schedule.
--
-- /See:/ 'batchScheduleActionCreateResult' smart constructor.
newtype BatchScheduleActionCreateResult = BatchScheduleActionCreateResult'{_bScheduleActions
                                                                           ::
                                                                           [ScheduleAction]}
                                            deriving (Eq, Read, Show, Data,
                                                      Typeable, Generic)

-- | Creates a value of 'BatchScheduleActionCreateResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bScheduleActions' - List of actions that have been created in the schedule.
batchScheduleActionCreateResult
    :: BatchScheduleActionCreateResult
batchScheduleActionCreateResult
  = BatchScheduleActionCreateResult'{_bScheduleActions
                                       = mempty}

-- | List of actions that have been created in the schedule.
bScheduleActions :: Lens' BatchScheduleActionCreateResult [ScheduleAction]
bScheduleActions = lens _bScheduleActions (\ s a -> s{_bScheduleActions = a}) . _Coerce

instance FromJSON BatchScheduleActionCreateResult
         where
        parseJSON
          = withObject "BatchScheduleActionCreateResult"
              (\ x ->
                 BatchScheduleActionCreateResult' <$>
                   (x .:? "scheduleActions" .!= mempty))

instance Hashable BatchScheduleActionCreateResult
         where

instance NFData BatchScheduleActionCreateResult where

-- | A list of schedule actions to delete.
--
-- /See:/ 'batchScheduleActionDeleteRequest' smart constructor.
newtype BatchScheduleActionDeleteRequest = BatchScheduleActionDeleteRequest'{_bsadrActionNames
                                                                             ::
                                                                             [Text]}
                                             deriving (Eq, Read, Show, Data,
                                                       Typeable, Generic)

-- | Creates a value of 'BatchScheduleActionDeleteRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsadrActionNames' - A list of schedule actions to delete.
batchScheduleActionDeleteRequest
    :: BatchScheduleActionDeleteRequest
batchScheduleActionDeleteRequest
  = BatchScheduleActionDeleteRequest'{_bsadrActionNames
                                        = mempty}

-- | A list of schedule actions to delete.
bsadrActionNames :: Lens' BatchScheduleActionDeleteRequest [Text]
bsadrActionNames = lens _bsadrActionNames (\ s a -> s{_bsadrActionNames = a}) . _Coerce

instance Hashable BatchScheduleActionDeleteRequest
         where

instance NFData BatchScheduleActionDeleteRequest
         where

instance ToJSON BatchScheduleActionDeleteRequest
         where
        toJSON BatchScheduleActionDeleteRequest'{..}
          = object
              (catMaybes
                 [Just ("actionNames" .= _bsadrActionNames)])

-- | List of actions that have been deleted from the schedule.
--
-- /See:/ 'batchScheduleActionDeleteResult' smart constructor.
newtype BatchScheduleActionDeleteResult = BatchScheduleActionDeleteResult'{_bsadrScheduleActions
                                                                           ::
                                                                           [ScheduleAction]}
                                            deriving (Eq, Read, Show, Data,
                                                      Typeable, Generic)

-- | Creates a value of 'BatchScheduleActionDeleteResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsadrScheduleActions' - List of actions that have been deleted from the schedule.
batchScheduleActionDeleteResult
    :: BatchScheduleActionDeleteResult
batchScheduleActionDeleteResult
  = BatchScheduleActionDeleteResult'{_bsadrScheduleActions
                                       = mempty}

-- | List of actions that have been deleted from the schedule.
bsadrScheduleActions :: Lens' BatchScheduleActionDeleteResult [ScheduleAction]
bsadrScheduleActions = lens _bsadrScheduleActions (\ s a -> s{_bsadrScheduleActions = a}) . _Coerce

instance FromJSON BatchScheduleActionDeleteResult
         where
        parseJSON
          = withObject "BatchScheduleActionDeleteResult"
              (\ x ->
                 BatchScheduleActionDeleteResult' <$>
                   (x .:? "scheduleActions" .!= mempty))

instance Hashable BatchScheduleActionDeleteResult
         where

instance NFData BatchScheduleActionDeleteResult where

-- | Blackout Slate
--
-- /See:/ 'blackoutSlate' smart constructor.
data BlackoutSlate = BlackoutSlate'{_bsNetworkEndBlackoutImage
                                    :: !(Maybe InputLocation),
                                    _bsState :: !(Maybe BlackoutSlateState),
                                    _bsNetworkEndBlackout ::
                                    !(Maybe BlackoutSlateNetworkEndBlackout),
                                    _bsNetworkId :: !(Maybe Text),
                                    _bsBlackoutSlateImage ::
                                    !(Maybe InputLocation)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BlackoutSlate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsNetworkEndBlackoutImage' - Path to local file to use as Network End Blackout image. Image will be scaled to fill the entire output raster.
--
-- * 'bsState' - When set to enabled, causes video, audio and captions to be blanked when indicated by program metadata.
--
-- * 'bsNetworkEndBlackout' - Setting to enabled causes the encoder to blackout the video, audio, and captions, and raise the "Network Blackout Image" slate when an SCTE104/35 Network End Segmentation Descriptor is encountered. The blackout will be lifted when the Network Start Segmentation Descriptor is encountered. The Network End and Network Start descriptors must contain a network ID that matches the value entered in "Network ID".
--
-- * 'bsNetworkId' - Provides Network ID that matches EIDR ID format (e.g., "10.XXXX/XXXX-XXXX-XXXX-XXXX-XXXX-C").
--
-- * 'bsBlackoutSlateImage' - Blackout slate image to be used. Leave empty for solid black. Only bmp and png images are supported.
blackoutSlate
    :: BlackoutSlate
blackoutSlate
  = BlackoutSlate'{_bsNetworkEndBlackoutImage =
                     Nothing,
                   _bsState = Nothing, _bsNetworkEndBlackout = Nothing,
                   _bsNetworkId = Nothing,
                   _bsBlackoutSlateImage = Nothing}

-- | Path to local file to use as Network End Blackout image. Image will be scaled to fill the entire output raster.
bsNetworkEndBlackoutImage :: Lens' BlackoutSlate (Maybe InputLocation)
bsNetworkEndBlackoutImage = lens _bsNetworkEndBlackoutImage (\ s a -> s{_bsNetworkEndBlackoutImage = a})

-- | When set to enabled, causes video, audio and captions to be blanked when indicated by program metadata.
bsState :: Lens' BlackoutSlate (Maybe BlackoutSlateState)
bsState = lens _bsState (\ s a -> s{_bsState = a})

-- | Setting to enabled causes the encoder to blackout the video, audio, and captions, and raise the "Network Blackout Image" slate when an SCTE104/35 Network End Segmentation Descriptor is encountered. The blackout will be lifted when the Network Start Segmentation Descriptor is encountered. The Network End and Network Start descriptors must contain a network ID that matches the value entered in "Network ID".
bsNetworkEndBlackout :: Lens' BlackoutSlate (Maybe BlackoutSlateNetworkEndBlackout)
bsNetworkEndBlackout = lens _bsNetworkEndBlackout (\ s a -> s{_bsNetworkEndBlackout = a})

-- | Provides Network ID that matches EIDR ID format (e.g., "10.XXXX/XXXX-XXXX-XXXX-XXXX-XXXX-C").
bsNetworkId :: Lens' BlackoutSlate (Maybe Text)
bsNetworkId = lens _bsNetworkId (\ s a -> s{_bsNetworkId = a})

-- | Blackout slate image to be used. Leave empty for solid black. Only bmp and png images are supported.
bsBlackoutSlateImage :: Lens' BlackoutSlate (Maybe InputLocation)
bsBlackoutSlateImage = lens _bsBlackoutSlateImage (\ s a -> s{_bsBlackoutSlateImage = a})

instance FromJSON BlackoutSlate where
        parseJSON
          = withObject "BlackoutSlate"
              (\ x ->
                 BlackoutSlate' <$>
                   (x .:? "networkEndBlackoutImage") <*> (x .:? "state")
                     <*> (x .:? "networkEndBlackout")
                     <*> (x .:? "networkId")
                     <*> (x .:? "blackoutSlateImage"))

instance Hashable BlackoutSlate where

instance NFData BlackoutSlate where

instance ToJSON BlackoutSlate where
        toJSON BlackoutSlate'{..}
          = object
              (catMaybes
                 [("networkEndBlackoutImage" .=) <$>
                    _bsNetworkEndBlackoutImage,
                  ("state" .=) <$> _bsState,
                  ("networkEndBlackout" .=) <$> _bsNetworkEndBlackout,
                  ("networkId" .=) <$> _bsNetworkId,
                  ("blackoutSlateImage" .=) <$> _bsBlackoutSlateImage])

-- | Burn In Destination Settings
--
-- /See:/ 'burnInDestinationSettings' smart constructor.
data BurnInDestinationSettings = BurnInDestinationSettings'{_bidsBackgroundOpacity
                                                            :: !(Maybe Nat),
                                                            _bidsFontOpacity ::
                                                            !(Maybe Nat),
                                                            _bidsShadowYOffset
                                                            :: !(Maybe Int),
                                                            _bidsFontResolution
                                                            :: !(Maybe Nat),
                                                            _bidsYPosition ::
                                                            !(Maybe Nat),
                                                            _bidsBackgroundColor
                                                            ::
                                                            !(Maybe
                                                                BurnInBackgroundColor),
                                                            _bidsShadowXOffset
                                                            :: !(Maybe Int),
                                                            _bidsFontSize ::
                                                            !(Maybe Text),
                                                            _bidsXPosition ::
                                                            !(Maybe Nat),
                                                            _bidsAlignment ::
                                                            !(Maybe
                                                                BurnInAlignment),
                                                            _bidsShadowOpacity
                                                            :: !(Maybe Nat),
                                                            _bidsTeletextGridControl
                                                            ::
                                                            !(Maybe
                                                                BurnInTeletextGridControl),
                                                            _bidsOutlineColor ::
                                                            !(Maybe
                                                                BurnInOutlineColor),
                                                            _bidsOutlineSize ::
                                                            !(Maybe Nat),
                                                            _bidsFont ::
                                                            !(Maybe
                                                                InputLocation),
                                                            _bidsShadowColor ::
                                                            !(Maybe
                                                                BurnInShadowColor),
                                                            _bidsFontColor ::
                                                            !(Maybe
                                                                BurnInFontColor)}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'BurnInDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bidsBackgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter out is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsFontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsShadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsFontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsYPosition' - Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit yPosition is provided, the caption will be positioned towards the bottom of the output.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsBackgroundColor' - Specifies the color of the rectangle behind the captions.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsShadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsFontSize' - When set to 'auto' fontSize will scale depending on the size of the output.  Giving a positive integer will specify the exact font size in points.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsXPosition' - Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit xPosition is provided, the horizontal caption position will be determined by the alignment parameter.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsAlignment' - If no explicit xPosition or yPosition is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. Selecting "smart" justification will left-justify live subtitles and center-justify pre-recorded subtitles.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsShadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter out is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsTeletextGridControl' - Controls whether a fixed grid size will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
--
-- * 'bidsOutlineColor' - Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsOutlineSize' - Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsFont' - External font file used for caption burn-in. File extension must be 'ttf' or 'tte'.  Although the user can select output fonts for many different types of input captions,  embedded, STL and teletext sources use a strict grid system. Using external fonts with these caption sources could cause unexpected display of proportional fonts.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsShadowColor' - Specifies the color of the shadow cast by the captions.  All burn-in and DVB-Sub font settings must match.
--
-- * 'bidsFontColor' - Specifies the color of the burned-in captions.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
burnInDestinationSettings
    :: BurnInDestinationSettings
burnInDestinationSettings
  = BurnInDestinationSettings'{_bidsBackgroundOpacity =
                                 Nothing,
                               _bidsFontOpacity = Nothing,
                               _bidsShadowYOffset = Nothing,
                               _bidsFontResolution = Nothing,
                               _bidsYPosition = Nothing,
                               _bidsBackgroundColor = Nothing,
                               _bidsShadowXOffset = Nothing,
                               _bidsFontSize = Nothing,
                               _bidsXPosition = Nothing,
                               _bidsAlignment = Nothing,
                               _bidsShadowOpacity = Nothing,
                               _bidsTeletextGridControl = Nothing,
                               _bidsOutlineColor = Nothing,
                               _bidsOutlineSize = Nothing, _bidsFont = Nothing,
                               _bidsShadowColor = Nothing,
                               _bidsFontColor = Nothing}

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter out is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
bidsBackgroundOpacity :: Lens' BurnInDestinationSettings (Maybe Natural)
bidsBackgroundOpacity = lens _bidsBackgroundOpacity (\ s a -> s{_bidsBackgroundOpacity = a}) . mapping _Nat

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.  All burn-in and DVB-Sub font settings must match.
bidsFontOpacity :: Lens' BurnInDestinationSettings (Maybe Natural)
bidsFontOpacity = lens _bidsFontOpacity (\ s a -> s{_bidsFontOpacity = a}) . mapping _Nat

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text.  All burn-in and DVB-Sub font settings must match.
bidsShadowYOffset :: Lens' BurnInDestinationSettings (Maybe Int)
bidsShadowYOffset = lens _bidsShadowYOffset (\ s a -> s{_bidsShadowYOffset = a})

-- | Font resolution in DPI (dots per inch); default is 96 dpi.  All burn-in and DVB-Sub font settings must match.
bidsFontResolution :: Lens' BurnInDestinationSettings (Maybe Natural)
bidsFontResolution = lens _bidsFontResolution (\ s a -> s{_bidsFontResolution = a}) . mapping _Nat

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit yPosition is provided, the caption will be positioned towards the bottom of the output.  All burn-in and DVB-Sub font settings must match.
bidsYPosition :: Lens' BurnInDestinationSettings (Maybe Natural)
bidsYPosition = lens _bidsYPosition (\ s a -> s{_bidsYPosition = a}) . mapping _Nat

-- | Specifies the color of the rectangle behind the captions.  All burn-in and DVB-Sub font settings must match.
bidsBackgroundColor :: Lens' BurnInDestinationSettings (Maybe BurnInBackgroundColor)
bidsBackgroundColor = lens _bidsBackgroundColor (\ s a -> s{_bidsBackgroundColor = a})

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left.  All burn-in and DVB-Sub font settings must match.
bidsShadowXOffset :: Lens' BurnInDestinationSettings (Maybe Int)
bidsShadowXOffset = lens _bidsShadowXOffset (\ s a -> s{_bidsShadowXOffset = a})

-- | When set to 'auto' fontSize will scale depending on the size of the output.  Giving a positive integer will specify the exact font size in points.  All burn-in and DVB-Sub font settings must match.
bidsFontSize :: Lens' BurnInDestinationSettings (Maybe Text)
bidsFontSize = lens _bidsFontSize (\ s a -> s{_bidsFontSize = a})

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit xPosition is provided, the horizontal caption position will be determined by the alignment parameter.  All burn-in and DVB-Sub font settings must match.
bidsXPosition :: Lens' BurnInDestinationSettings (Maybe Natural)
bidsXPosition = lens _bidsXPosition (\ s a -> s{_bidsXPosition = a}) . mapping _Nat

-- | If no explicit xPosition or yPosition is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. Selecting "smart" justification will left-justify live subtitles and center-justify pre-recorded subtitles.  All burn-in and DVB-Sub font settings must match.
bidsAlignment :: Lens' BurnInDestinationSettings (Maybe BurnInAlignment)
bidsAlignment = lens _bidsAlignment (\ s a -> s{_bidsAlignment = a})

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter out is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
bidsShadowOpacity :: Lens' BurnInDestinationSettings (Maybe Natural)
bidsShadowOpacity = lens _bidsShadowOpacity (\ s a -> s{_bidsShadowOpacity = a}) . mapping _Nat

-- | Controls whether a fixed grid size will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
bidsTeletextGridControl :: Lens' BurnInDestinationSettings (Maybe BurnInTeletextGridControl)
bidsTeletextGridControl = lens _bidsTeletextGridControl (\ s a -> s{_bidsTeletextGridControl = a})

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bidsOutlineColor :: Lens' BurnInDestinationSettings (Maybe BurnInOutlineColor)
bidsOutlineColor = lens _bidsOutlineColor (\ s a -> s{_bidsOutlineColor = a})

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bidsOutlineSize :: Lens' BurnInDestinationSettings (Maybe Natural)
bidsOutlineSize = lens _bidsOutlineSize (\ s a -> s{_bidsOutlineSize = a}) . mapping _Nat

-- | External font file used for caption burn-in. File extension must be 'ttf' or 'tte'.  Although the user can select output fonts for many different types of input captions,  embedded, STL and teletext sources use a strict grid system. Using external fonts with these caption sources could cause unexpected display of proportional fonts.  All burn-in and DVB-Sub font settings must match.
bidsFont :: Lens' BurnInDestinationSettings (Maybe InputLocation)
bidsFont = lens _bidsFont (\ s a -> s{_bidsFont = a})

-- | Specifies the color of the shadow cast by the captions.  All burn-in and DVB-Sub font settings must match.
bidsShadowColor :: Lens' BurnInDestinationSettings (Maybe BurnInShadowColor)
bidsShadowColor = lens _bidsShadowColor (\ s a -> s{_bidsShadowColor = a})

-- | Specifies the color of the burned-in captions.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
bidsFontColor :: Lens' BurnInDestinationSettings (Maybe BurnInFontColor)
bidsFontColor = lens _bidsFontColor (\ s a -> s{_bidsFontColor = a})

instance FromJSON BurnInDestinationSettings where
        parseJSON
          = withObject "BurnInDestinationSettings"
              (\ x ->
                 BurnInDestinationSettings' <$>
                   (x .:? "backgroundOpacity") <*> (x .:? "fontOpacity")
                     <*> (x .:? "shadowYOffset")
                     <*> (x .:? "fontResolution")
                     <*> (x .:? "yPosition")
                     <*> (x .:? "backgroundColor")
                     <*> (x .:? "shadowXOffset")
                     <*> (x .:? "fontSize")
                     <*> (x .:? "xPosition")
                     <*> (x .:? "alignment")
                     <*> (x .:? "shadowOpacity")
                     <*> (x .:? "teletextGridControl")
                     <*> (x .:? "outlineColor")
                     <*> (x .:? "outlineSize")
                     <*> (x .:? "font")
                     <*> (x .:? "shadowColor")
                     <*> (x .:? "fontColor"))

instance Hashable BurnInDestinationSettings where

instance NFData BurnInDestinationSettings where

instance ToJSON BurnInDestinationSettings where
        toJSON BurnInDestinationSettings'{..}
          = object
              (catMaybes
                 [("backgroundOpacity" .=) <$> _bidsBackgroundOpacity,
                  ("fontOpacity" .=) <$> _bidsFontOpacity,
                  ("shadowYOffset" .=) <$> _bidsShadowYOffset,
                  ("fontResolution" .=) <$> _bidsFontResolution,
                  ("yPosition" .=) <$> _bidsYPosition,
                  ("backgroundColor" .=) <$> _bidsBackgroundColor,
                  ("shadowXOffset" .=) <$> _bidsShadowXOffset,
                  ("fontSize" .=) <$> _bidsFontSize,
                  ("xPosition" .=) <$> _bidsXPosition,
                  ("alignment" .=) <$> _bidsAlignment,
                  ("shadowOpacity" .=) <$> _bidsShadowOpacity,
                  ("teletextGridControl" .=) <$>
                    _bidsTeletextGridControl,
                  ("outlineColor" .=) <$> _bidsOutlineColor,
                  ("outlineSize" .=) <$> _bidsOutlineSize,
                  ("font" .=) <$> _bidsFont,
                  ("shadowColor" .=) <$> _bidsShadowColor,
                  ("fontColor" .=) <$> _bidsFontColor])

-- | Caption Description
--
-- /See:/ 'captionDescription' smart constructor.
data CaptionDescription = CaptionDescription'{_cdLanguageCode
                                              :: !(Maybe Text),
                                              _cdDestinationSettings ::
                                              !(Maybe
                                                  CaptionDestinationSettings),
                                              _cdLanguageDescription ::
                                              !(Maybe Text),
                                              _cdCaptionSelectorName :: !Text,
                                              _cdName :: !Text}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdLanguageCode' - ISO 639-2 three-digit code: http://www.loc.gov/standards/iso639-2/
--
-- * 'cdDestinationSettings' - Additional settings for captions destination that depend on the destination type.
--
-- * 'cdLanguageDescription' - Human readable information to indicate captions available for players (eg. English, or Spanish).
--
-- * 'cdCaptionSelectorName' - Specifies which input caption selector to use as a caption source when generating output captions. This field should match a captionSelector name.
--
-- * 'cdName' - Name of the caption description.  Used to associate a caption description with an output.  Names must be unique within an event.
captionDescription
    :: Text -- ^ 'cdCaptionSelectorName'
    -> Text -- ^ 'cdName'
    -> CaptionDescription
captionDescription pCaptionSelectorName_ pName_
  = CaptionDescription'{_cdLanguageCode = Nothing,
                        _cdDestinationSettings = Nothing,
                        _cdLanguageDescription = Nothing,
                        _cdCaptionSelectorName = pCaptionSelectorName_,
                        _cdName = pName_}

-- | ISO 639-2 three-digit code: http://www.loc.gov/standards/iso639-2/
cdLanguageCode :: Lens' CaptionDescription (Maybe Text)
cdLanguageCode = lens _cdLanguageCode (\ s a -> s{_cdLanguageCode = a})

-- | Additional settings for captions destination that depend on the destination type.
cdDestinationSettings :: Lens' CaptionDescription (Maybe CaptionDestinationSettings)
cdDestinationSettings = lens _cdDestinationSettings (\ s a -> s{_cdDestinationSettings = a})

-- | Human readable information to indicate captions available for players (eg. English, or Spanish).
cdLanguageDescription :: Lens' CaptionDescription (Maybe Text)
cdLanguageDescription = lens _cdLanguageDescription (\ s a -> s{_cdLanguageDescription = a})

-- | Specifies which input caption selector to use as a caption source when generating output captions. This field should match a captionSelector name.
cdCaptionSelectorName :: Lens' CaptionDescription Text
cdCaptionSelectorName = lens _cdCaptionSelectorName (\ s a -> s{_cdCaptionSelectorName = a})

-- | Name of the caption description.  Used to associate a caption description with an output.  Names must be unique within an event.
cdName :: Lens' CaptionDescription Text
cdName = lens _cdName (\ s a -> s{_cdName = a})

instance FromJSON CaptionDescription where
        parseJSON
          = withObject "CaptionDescription"
              (\ x ->
                 CaptionDescription' <$>
                   (x .:? "languageCode") <*>
                     (x .:? "destinationSettings")
                     <*> (x .:? "languageDescription")
                     <*> (x .: "captionSelectorName")
                     <*> (x .: "name"))

instance Hashable CaptionDescription where

instance NFData CaptionDescription where

instance ToJSON CaptionDescription where
        toJSON CaptionDescription'{..}
          = object
              (catMaybes
                 [("languageCode" .=) <$> _cdLanguageCode,
                  ("destinationSettings" .=) <$>
                    _cdDestinationSettings,
                  ("languageDescription" .=) <$>
                    _cdLanguageDescription,
                  Just
                    ("captionSelectorName" .= _cdCaptionSelectorName),
                  Just ("name" .= _cdName)])

-- | Caption Destination Settings
--
-- /See:/ 'captionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'{_cdsTeletextDestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  TeletextDestinationSettings),
                                                              _cdsRtmpCaptionInfoDestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  RtmpCaptionInfoDestinationSettings),
                                                              _cdsDvbSubDestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  DvbSubDestinationSettings),
                                                              _cdsScte27DestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  Scte27DestinationSettings),
                                                              _cdsTtmlDestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  TtmlDestinationSettings),
                                                              _cdsScte20PlusEmbeddedDestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  Scte20PlusEmbeddedDestinationSettings),
                                                              _cdsEmbeddedPlusScte20DestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  EmbeddedPlusScte20DestinationSettings),
                                                              _cdsSmpteTtDestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  SmpteTtDestinationSettings),
                                                              _cdsWebvttDestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  WebvttDestinationSettings),
                                                              _cdsEmbeddedDestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  EmbeddedDestinationSettings),
                                                              _cdsBurnInDestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  BurnInDestinationSettings),
                                                              _cdsAribDestinationSettings
                                                              ::
                                                              !(Maybe
                                                                  AribDestinationSettings)}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'CaptionDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsTeletextDestinationSettings' - Undocumented member.
--
-- * 'cdsRtmpCaptionInfoDestinationSettings' - Undocumented member.
--
-- * 'cdsDvbSubDestinationSettings' - Undocumented member.
--
-- * 'cdsScte27DestinationSettings' - Undocumented member.
--
-- * 'cdsTtmlDestinationSettings' - Undocumented member.
--
-- * 'cdsScte20PlusEmbeddedDestinationSettings' - Undocumented member.
--
-- * 'cdsEmbeddedPlusScte20DestinationSettings' - Undocumented member.
--
-- * 'cdsSmpteTtDestinationSettings' - Undocumented member.
--
-- * 'cdsWebvttDestinationSettings' - Undocumented member.
--
-- * 'cdsEmbeddedDestinationSettings' - Undocumented member.
--
-- * 'cdsBurnInDestinationSettings' - Undocumented member.
--
-- * 'cdsAribDestinationSettings' - Undocumented member.
captionDestinationSettings
    :: CaptionDestinationSettings
captionDestinationSettings
  = CaptionDestinationSettings'{_cdsTeletextDestinationSettings
                                  = Nothing,
                                _cdsRtmpCaptionInfoDestinationSettings =
                                  Nothing,
                                _cdsDvbSubDestinationSettings = Nothing,
                                _cdsScte27DestinationSettings = Nothing,
                                _cdsTtmlDestinationSettings = Nothing,
                                _cdsScte20PlusEmbeddedDestinationSettings =
                                  Nothing,
                                _cdsEmbeddedPlusScte20DestinationSettings =
                                  Nothing,
                                _cdsSmpteTtDestinationSettings = Nothing,
                                _cdsWebvttDestinationSettings = Nothing,
                                _cdsEmbeddedDestinationSettings = Nothing,
                                _cdsBurnInDestinationSettings = Nothing,
                                _cdsAribDestinationSettings = Nothing}

-- | Undocumented member.
cdsTeletextDestinationSettings :: Lens' CaptionDestinationSettings (Maybe TeletextDestinationSettings)
cdsTeletextDestinationSettings = lens _cdsTeletextDestinationSettings (\ s a -> s{_cdsTeletextDestinationSettings = a})

-- | Undocumented member.
cdsRtmpCaptionInfoDestinationSettings :: Lens' CaptionDestinationSettings (Maybe RtmpCaptionInfoDestinationSettings)
cdsRtmpCaptionInfoDestinationSettings = lens _cdsRtmpCaptionInfoDestinationSettings (\ s a -> s{_cdsRtmpCaptionInfoDestinationSettings = a})

-- | Undocumented member.
cdsDvbSubDestinationSettings :: Lens' CaptionDestinationSettings (Maybe DvbSubDestinationSettings)
cdsDvbSubDestinationSettings = lens _cdsDvbSubDestinationSettings (\ s a -> s{_cdsDvbSubDestinationSettings = a})

-- | Undocumented member.
cdsScte27DestinationSettings :: Lens' CaptionDestinationSettings (Maybe Scte27DestinationSettings)
cdsScte27DestinationSettings = lens _cdsScte27DestinationSettings (\ s a -> s{_cdsScte27DestinationSettings = a})

-- | Undocumented member.
cdsTtmlDestinationSettings :: Lens' CaptionDestinationSettings (Maybe TtmlDestinationSettings)
cdsTtmlDestinationSettings = lens _cdsTtmlDestinationSettings (\ s a -> s{_cdsTtmlDestinationSettings = a})

-- | Undocumented member.
cdsScte20PlusEmbeddedDestinationSettings :: Lens' CaptionDestinationSettings (Maybe Scte20PlusEmbeddedDestinationSettings)
cdsScte20PlusEmbeddedDestinationSettings = lens _cdsScte20PlusEmbeddedDestinationSettings (\ s a -> s{_cdsScte20PlusEmbeddedDestinationSettings = a})

-- | Undocumented member.
cdsEmbeddedPlusScte20DestinationSettings :: Lens' CaptionDestinationSettings (Maybe EmbeddedPlusScte20DestinationSettings)
cdsEmbeddedPlusScte20DestinationSettings = lens _cdsEmbeddedPlusScte20DestinationSettings (\ s a -> s{_cdsEmbeddedPlusScte20DestinationSettings = a})

-- | Undocumented member.
cdsSmpteTtDestinationSettings :: Lens' CaptionDestinationSettings (Maybe SmpteTtDestinationSettings)
cdsSmpteTtDestinationSettings = lens _cdsSmpteTtDestinationSettings (\ s a -> s{_cdsSmpteTtDestinationSettings = a})

-- | Undocumented member.
cdsWebvttDestinationSettings :: Lens' CaptionDestinationSettings (Maybe WebvttDestinationSettings)
cdsWebvttDestinationSettings = lens _cdsWebvttDestinationSettings (\ s a -> s{_cdsWebvttDestinationSettings = a})

-- | Undocumented member.
cdsEmbeddedDestinationSettings :: Lens' CaptionDestinationSettings (Maybe EmbeddedDestinationSettings)
cdsEmbeddedDestinationSettings = lens _cdsEmbeddedDestinationSettings (\ s a -> s{_cdsEmbeddedDestinationSettings = a})

-- | Undocumented member.
cdsBurnInDestinationSettings :: Lens' CaptionDestinationSettings (Maybe BurnInDestinationSettings)
cdsBurnInDestinationSettings = lens _cdsBurnInDestinationSettings (\ s a -> s{_cdsBurnInDestinationSettings = a})

-- | Undocumented member.
cdsAribDestinationSettings :: Lens' CaptionDestinationSettings (Maybe AribDestinationSettings)
cdsAribDestinationSettings = lens _cdsAribDestinationSettings (\ s a -> s{_cdsAribDestinationSettings = a})

instance FromJSON CaptionDestinationSettings where
        parseJSON
          = withObject "CaptionDestinationSettings"
              (\ x ->
                 CaptionDestinationSettings' <$>
                   (x .:? "teletextDestinationSettings") <*>
                     (x .:? "rtmpCaptionInfoDestinationSettings")
                     <*> (x .:? "dvbSubDestinationSettings")
                     <*> (x .:? "scte27DestinationSettings")
                     <*> (x .:? "ttmlDestinationSettings")
                     <*> (x .:? "scte20PlusEmbeddedDestinationSettings")
                     <*> (x .:? "embeddedPlusScte20DestinationSettings")
                     <*> (x .:? "smpteTtDestinationSettings")
                     <*> (x .:? "webvttDestinationSettings")
                     <*> (x .:? "embeddedDestinationSettings")
                     <*> (x .:? "burnInDestinationSettings")
                     <*> (x .:? "aribDestinationSettings"))

instance Hashable CaptionDestinationSettings where

instance NFData CaptionDestinationSettings where

instance ToJSON CaptionDestinationSettings where
        toJSON CaptionDestinationSettings'{..}
          = object
              (catMaybes
                 [("teletextDestinationSettings" .=) <$>
                    _cdsTeletextDestinationSettings,
                  ("rtmpCaptionInfoDestinationSettings" .=) <$>
                    _cdsRtmpCaptionInfoDestinationSettings,
                  ("dvbSubDestinationSettings" .=) <$>
                    _cdsDvbSubDestinationSettings,
                  ("scte27DestinationSettings" .=) <$>
                    _cdsScte27DestinationSettings,
                  ("ttmlDestinationSettings" .=) <$>
                    _cdsTtmlDestinationSettings,
                  ("scte20PlusEmbeddedDestinationSettings" .=) <$>
                    _cdsScte20PlusEmbeddedDestinationSettings,
                  ("embeddedPlusScte20DestinationSettings" .=) <$>
                    _cdsEmbeddedPlusScte20DestinationSettings,
                  ("smpteTtDestinationSettings" .=) <$>
                    _cdsSmpteTtDestinationSettings,
                  ("webvttDestinationSettings" .=) <$>
                    _cdsWebvttDestinationSettings,
                  ("embeddedDestinationSettings" .=) <$>
                    _cdsEmbeddedDestinationSettings,
                  ("burnInDestinationSettings" .=) <$>
                    _cdsBurnInDestinationSettings,
                  ("aribDestinationSettings" .=) <$>
                    _cdsAribDestinationSettings])

-- | Maps a caption channel to an ISO 693-2 language code (http://www.loc.gov/standards/iso639-2), with an optional description.
--
-- /See:/ 'captionLanguageMapping' smart constructor.
data CaptionLanguageMapping = CaptionLanguageMapping'{_clmLanguageCode
                                                      :: !Text,
                                                      _clmLanguageDescription ::
                                                      !Text,
                                                      _clmCaptionChannel ::
                                                      !Nat}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'CaptionLanguageMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clmLanguageCode' - Three character ISO 639-2 language code (see http://www.loc.gov/standards/iso639-2)
--
-- * 'clmLanguageDescription' - Textual description of language
--
-- * 'clmCaptionChannel' - The closed caption channel being described by this CaptionLanguageMapping.  Each channel mapping must have a unique channel number (maximum of 4)
captionLanguageMapping
    :: Text -- ^ 'clmLanguageCode'
    -> Text -- ^ 'clmLanguageDescription'
    -> Natural -- ^ 'clmCaptionChannel'
    -> CaptionLanguageMapping
captionLanguageMapping pLanguageCode_
  pLanguageDescription_ pCaptionChannel_
  = CaptionLanguageMapping'{_clmLanguageCode =
                              pLanguageCode_,
                            _clmLanguageDescription = pLanguageDescription_,
                            _clmCaptionChannel = _Nat # pCaptionChannel_}

-- | Three character ISO 639-2 language code (see http://www.loc.gov/standards/iso639-2)
clmLanguageCode :: Lens' CaptionLanguageMapping Text
clmLanguageCode = lens _clmLanguageCode (\ s a -> s{_clmLanguageCode = a})

-- | Textual description of language
clmLanguageDescription :: Lens' CaptionLanguageMapping Text
clmLanguageDescription = lens _clmLanguageDescription (\ s a -> s{_clmLanguageDescription = a})

-- | The closed caption channel being described by this CaptionLanguageMapping.  Each channel mapping must have a unique channel number (maximum of 4)
clmCaptionChannel :: Lens' CaptionLanguageMapping Natural
clmCaptionChannel = lens _clmCaptionChannel (\ s a -> s{_clmCaptionChannel = a}) . _Nat

instance FromJSON CaptionLanguageMapping where
        parseJSON
          = withObject "CaptionLanguageMapping"
              (\ x ->
                 CaptionLanguageMapping' <$>
                   (x .: "languageCode") <*>
                     (x .: "languageDescription")
                     <*> (x .: "captionChannel"))

instance Hashable CaptionLanguageMapping where

instance NFData CaptionLanguageMapping where

instance ToJSON CaptionLanguageMapping where
        toJSON CaptionLanguageMapping'{..}
          = object
              (catMaybes
                 [Just ("languageCode" .= _clmLanguageCode),
                  Just
                    ("languageDescription" .= _clmLanguageDescription),
                  Just ("captionChannel" .= _clmCaptionChannel)])

-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
--
-- /See:/ 'captionSelector' smart constructor.
data CaptionSelector = CaptionSelector'{_csLanguageCode
                                        :: !(Maybe Text),
                                        _csSelectorSettings ::
                                        !(Maybe CaptionSelectorSettings),
                                        _csName :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptionSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csLanguageCode' - When specified this field indicates the three letter language code of the caption track to extract from the source.
--
-- * 'csSelectorSettings' - Caption selector settings.
--
-- * 'csName' - Name identifier for a caption selector.  This name is used to associate this caption selector with one or more caption descriptions.  Names must be unique within an event.
captionSelector
    :: Text -- ^ 'csName'
    -> CaptionSelector
captionSelector pName_
  = CaptionSelector'{_csLanguageCode = Nothing,
                     _csSelectorSettings = Nothing, _csName = pName_}

-- | When specified this field indicates the three letter language code of the caption track to extract from the source.
csLanguageCode :: Lens' CaptionSelector (Maybe Text)
csLanguageCode = lens _csLanguageCode (\ s a -> s{_csLanguageCode = a})

-- | Caption selector settings.
csSelectorSettings :: Lens' CaptionSelector (Maybe CaptionSelectorSettings)
csSelectorSettings = lens _csSelectorSettings (\ s a -> s{_csSelectorSettings = a})

-- | Name identifier for a caption selector.  This name is used to associate this caption selector with one or more caption descriptions.  Names must be unique within an event.
csName :: Lens' CaptionSelector Text
csName = lens _csName (\ s a -> s{_csName = a})

instance FromJSON CaptionSelector where
        parseJSON
          = withObject "CaptionSelector"
              (\ x ->
                 CaptionSelector' <$>
                   (x .:? "languageCode") <*> (x .:? "selectorSettings")
                     <*> (x .: "name"))

instance Hashable CaptionSelector where

instance NFData CaptionSelector where

instance ToJSON CaptionSelector where
        toJSON CaptionSelector'{..}
          = object
              (catMaybes
                 [("languageCode" .=) <$> _csLanguageCode,
                  ("selectorSettings" .=) <$> _csSelectorSettings,
                  Just ("name" .= _csName)])

-- | Caption Selector Settings
--
-- /See:/ 'captionSelectorSettings' smart constructor.
data CaptionSelectorSettings = CaptionSelectorSettings'{_cssTeletextSourceSettings
                                                        ::
                                                        !(Maybe
                                                            TeletextSourceSettings),
                                                        _cssAribSourceSettings
                                                        ::
                                                        !(Maybe
                                                            AribSourceSettings),
                                                        _cssScte27SourceSettings
                                                        ::
                                                        !(Maybe
                                                            Scte27SourceSettings),
                                                        _cssDvbSubSourceSettings
                                                        ::
                                                        !(Maybe
                                                            DvbSubSourceSettings),
                                                        _cssScte20SourceSettings
                                                        ::
                                                        !(Maybe
                                                            Scte20SourceSettings),
                                                        _cssEmbeddedSourceSettings
                                                        ::
                                                        !(Maybe
                                                            EmbeddedSourceSettings)}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'CaptionSelectorSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssTeletextSourceSettings' - Undocumented member.
--
-- * 'cssAribSourceSettings' - Undocumented member.
--
-- * 'cssScte27SourceSettings' - Undocumented member.
--
-- * 'cssDvbSubSourceSettings' - Undocumented member.
--
-- * 'cssScte20SourceSettings' - Undocumented member.
--
-- * 'cssEmbeddedSourceSettings' - Undocumented member.
captionSelectorSettings
    :: CaptionSelectorSettings
captionSelectorSettings
  = CaptionSelectorSettings'{_cssTeletextSourceSettings
                               = Nothing,
                             _cssAribSourceSettings = Nothing,
                             _cssScte27SourceSettings = Nothing,
                             _cssDvbSubSourceSettings = Nothing,
                             _cssScte20SourceSettings = Nothing,
                             _cssEmbeddedSourceSettings = Nothing}

-- | Undocumented member.
cssTeletextSourceSettings :: Lens' CaptionSelectorSettings (Maybe TeletextSourceSettings)
cssTeletextSourceSettings = lens _cssTeletextSourceSettings (\ s a -> s{_cssTeletextSourceSettings = a})

-- | Undocumented member.
cssAribSourceSettings :: Lens' CaptionSelectorSettings (Maybe AribSourceSettings)
cssAribSourceSettings = lens _cssAribSourceSettings (\ s a -> s{_cssAribSourceSettings = a})

-- | Undocumented member.
cssScte27SourceSettings :: Lens' CaptionSelectorSettings (Maybe Scte27SourceSettings)
cssScte27SourceSettings = lens _cssScte27SourceSettings (\ s a -> s{_cssScte27SourceSettings = a})

-- | Undocumented member.
cssDvbSubSourceSettings :: Lens' CaptionSelectorSettings (Maybe DvbSubSourceSettings)
cssDvbSubSourceSettings = lens _cssDvbSubSourceSettings (\ s a -> s{_cssDvbSubSourceSettings = a})

-- | Undocumented member.
cssScte20SourceSettings :: Lens' CaptionSelectorSettings (Maybe Scte20SourceSettings)
cssScte20SourceSettings = lens _cssScte20SourceSettings (\ s a -> s{_cssScte20SourceSettings = a})

-- | Undocumented member.
cssEmbeddedSourceSettings :: Lens' CaptionSelectorSettings (Maybe EmbeddedSourceSettings)
cssEmbeddedSourceSettings = lens _cssEmbeddedSourceSettings (\ s a -> s{_cssEmbeddedSourceSettings = a})

instance FromJSON CaptionSelectorSettings where
        parseJSON
          = withObject "CaptionSelectorSettings"
              (\ x ->
                 CaptionSelectorSettings' <$>
                   (x .:? "teletextSourceSettings") <*>
                     (x .:? "aribSourceSettings")
                     <*> (x .:? "scte27SourceSettings")
                     <*> (x .:? "dvbSubSourceSettings")
                     <*> (x .:? "scte20SourceSettings")
                     <*> (x .:? "embeddedSourceSettings"))

instance Hashable CaptionSelectorSettings where

instance NFData CaptionSelectorSettings where

instance ToJSON CaptionSelectorSettings where
        toJSON CaptionSelectorSettings'{..}
          = object
              (catMaybes
                 [("teletextSourceSettings" .=) <$>
                    _cssTeletextSourceSettings,
                  ("aribSourceSettings" .=) <$> _cssAribSourceSettings,
                  ("scte27SourceSettings" .=) <$>
                    _cssScte27SourceSettings,
                  ("dvbSubSourceSettings" .=) <$>
                    _cssDvbSubSourceSettings,
                  ("scte20SourceSettings" .=) <$>
                    _cssScte20SourceSettings,
                  ("embeddedSourceSettings" .=) <$>
                    _cssEmbeddedSourceSettings])

-- | Placeholder documentation for Channel
--
-- /See:/ 'channel' smart constructor.
data Channel = Channel'{_cState ::
                        !(Maybe ChannelState),
                        _cLogLevel :: !(Maybe LogLevel),
                        _cARN :: !(Maybe Text),
                        _cPipelinesRunningCount :: !(Maybe Int),
                        _cPipelineDetails :: !(Maybe [PipelineDetail]),
                        _cInputSpecification :: !(Maybe InputSpecification),
                        _cInputAttachments :: !(Maybe [InputAttachment]),
                        _cDestinations :: !(Maybe [OutputDestination]),
                        _cName :: !(Maybe Text), _cId :: !(Maybe Text),
                        _cChannelClass :: !(Maybe ChannelClass),
                        _cEgressEndpoints ::
                        !(Maybe [ChannelEgressEndpoint]),
                        _cTags :: !(Maybe (Map Text Text)),
                        _cEncoderSettings :: !(Maybe EncoderSettings),
                        _cRoleARN :: !(Maybe Text)}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cState' - Undocumented member.
--
-- * 'cLogLevel' - The log level being written to CloudWatch Logs.
--
-- * 'cARN' - The unique arn of the channel.
--
-- * 'cPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'cPipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- * 'cInputSpecification' - Undocumented member.
--
-- * 'cInputAttachments' - List of input attachments for channel.
--
-- * 'cDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'cName' - The name of the channel. (user-mutable)
--
-- * 'cId' - The unique id of the channel.
--
-- * 'cChannelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- * 'cEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'cTags' - A collection of key-value pairs.
--
-- * 'cEncoderSettings' - Undocumented member.
--
-- * 'cRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
channel
    :: Channel
channel
  = Channel'{_cState = Nothing, _cLogLevel = Nothing,
             _cARN = Nothing, _cPipelinesRunningCount = Nothing,
             _cPipelineDetails = Nothing,
             _cInputSpecification = Nothing,
             _cInputAttachments = Nothing,
             _cDestinations = Nothing, _cName = Nothing,
             _cId = Nothing, _cChannelClass = Nothing,
             _cEgressEndpoints = Nothing, _cTags = Nothing,
             _cEncoderSettings = Nothing, _cRoleARN = Nothing}

-- | Undocumented member.
cState :: Lens' Channel (Maybe ChannelState)
cState = lens _cState (\ s a -> s{_cState = a})

-- | The log level being written to CloudWatch Logs.
cLogLevel :: Lens' Channel (Maybe LogLevel)
cLogLevel = lens _cLogLevel (\ s a -> s{_cLogLevel = a})

-- | The unique arn of the channel.
cARN :: Lens' Channel (Maybe Text)
cARN = lens _cARN (\ s a -> s{_cARN = a})

-- | The number of currently healthy pipelines.
cPipelinesRunningCount :: Lens' Channel (Maybe Int)
cPipelinesRunningCount = lens _cPipelinesRunningCount (\ s a -> s{_cPipelinesRunningCount = a})

-- | Runtime details for the pipelines of a running channel.
cPipelineDetails :: Lens' Channel [PipelineDetail]
cPipelineDetails = lens _cPipelineDetails (\ s a -> s{_cPipelineDetails = a}) . _Default . _Coerce

-- | Undocumented member.
cInputSpecification :: Lens' Channel (Maybe InputSpecification)
cInputSpecification = lens _cInputSpecification (\ s a -> s{_cInputSpecification = a})

-- | List of input attachments for channel.
cInputAttachments :: Lens' Channel [InputAttachment]
cInputAttachments = lens _cInputAttachments (\ s a -> s{_cInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
cDestinations :: Lens' Channel [OutputDestination]
cDestinations = lens _cDestinations (\ s a -> s{_cDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
cName :: Lens' Channel (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

-- | The unique id of the channel.
cId :: Lens' Channel (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a})

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
cChannelClass :: Lens' Channel (Maybe ChannelClass)
cChannelClass = lens _cChannelClass (\ s a -> s{_cChannelClass = a})

-- | The endpoints where outgoing connections initiate from
cEgressEndpoints :: Lens' Channel [ChannelEgressEndpoint]
cEgressEndpoints = lens _cEgressEndpoints (\ s a -> s{_cEgressEndpoints = a}) . _Default . _Coerce

-- | A collection of key-value pairs.
cTags :: Lens' Channel (HashMap Text Text)
cTags = lens _cTags (\ s a -> s{_cTags = a}) . _Default . _Map

-- | Undocumented member.
cEncoderSettings :: Lens' Channel (Maybe EncoderSettings)
cEncoderSettings = lens _cEncoderSettings (\ s a -> s{_cEncoderSettings = a})

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
cRoleARN :: Lens' Channel (Maybe Text)
cRoleARN = lens _cRoleARN (\ s a -> s{_cRoleARN = a})

instance FromJSON Channel where
        parseJSON
          = withObject "Channel"
              (\ x ->
                 Channel' <$>
                   (x .:? "state") <*> (x .:? "logLevel") <*>
                     (x .:? "arn")
                     <*> (x .:? "pipelinesRunningCount")
                     <*> (x .:? "pipelineDetails" .!= mempty)
                     <*> (x .:? "inputSpecification")
                     <*> (x .:? "inputAttachments" .!= mempty)
                     <*> (x .:? "destinations" .!= mempty)
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "channelClass")
                     <*> (x .:? "egressEndpoints" .!= mempty)
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .:? "encoderSettings")
                     <*> (x .:? "roleArn"))

instance Hashable Channel where

instance NFData Channel where

-- | Placeholder documentation for ChannelEgressEndpoint
--
-- /See:/ 'channelEgressEndpoint' smart constructor.
newtype ChannelEgressEndpoint = ChannelEgressEndpoint'{_ceeSourceIP
                                                       :: Maybe Text}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'ChannelEgressEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceeSourceIP' - Public IP of where a channel's output comes from
channelEgressEndpoint
    :: ChannelEgressEndpoint
channelEgressEndpoint
  = ChannelEgressEndpoint'{_ceeSourceIP = Nothing}

-- | Public IP of where a channel's output comes from
ceeSourceIP :: Lens' ChannelEgressEndpoint (Maybe Text)
ceeSourceIP = lens _ceeSourceIP (\ s a -> s{_ceeSourceIP = a})

instance FromJSON ChannelEgressEndpoint where
        parseJSON
          = withObject "ChannelEgressEndpoint"
              (\ x ->
                 ChannelEgressEndpoint' <$> (x .:? "sourceIp"))

instance Hashable ChannelEgressEndpoint where

instance NFData ChannelEgressEndpoint where

-- | Placeholder documentation for ChannelSummary
--
-- /See:/ 'channelSummary' smart constructor.
data ChannelSummary = ChannelSummary'{_chaState ::
                                      !(Maybe ChannelState),
                                      _chaLogLevel :: !(Maybe LogLevel),
                                      _chaARN :: !(Maybe Text),
                                      _chaPipelinesRunningCount :: !(Maybe Int),
                                      _chaInputSpecification ::
                                      !(Maybe InputSpecification),
                                      _chaInputAttachments ::
                                      !(Maybe [InputAttachment]),
                                      _chaDestinations ::
                                      !(Maybe [OutputDestination]),
                                      _chaName :: !(Maybe Text),
                                      _chaId :: !(Maybe Text),
                                      _chaChannelClass :: !(Maybe ChannelClass),
                                      _chaEgressEndpoints ::
                                      !(Maybe [ChannelEgressEndpoint]),
                                      _chaTags :: !(Maybe (Map Text Text)),
                                      _chaRoleARN :: !(Maybe Text)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chaState' - Undocumented member.
--
-- * 'chaLogLevel' - The log level being written to CloudWatch Logs.
--
-- * 'chaARN' - The unique arn of the channel.
--
-- * 'chaPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'chaInputSpecification' - Undocumented member.
--
-- * 'chaInputAttachments' - List of input attachments for channel.
--
-- * 'chaDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'chaName' - The name of the channel. (user-mutable)
--
-- * 'chaId' - The unique id of the channel.
--
-- * 'chaChannelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- * 'chaEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'chaTags' - A collection of key-value pairs.
--
-- * 'chaRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
channelSummary
    :: ChannelSummary
channelSummary
  = ChannelSummary'{_chaState = Nothing,
                    _chaLogLevel = Nothing, _chaARN = Nothing,
                    _chaPipelinesRunningCount = Nothing,
                    _chaInputSpecification = Nothing,
                    _chaInputAttachments = Nothing,
                    _chaDestinations = Nothing, _chaName = Nothing,
                    _chaId = Nothing, _chaChannelClass = Nothing,
                    _chaEgressEndpoints = Nothing, _chaTags = Nothing,
                    _chaRoleARN = Nothing}

-- | Undocumented member.
chaState :: Lens' ChannelSummary (Maybe ChannelState)
chaState = lens _chaState (\ s a -> s{_chaState = a})

-- | The log level being written to CloudWatch Logs.
chaLogLevel :: Lens' ChannelSummary (Maybe LogLevel)
chaLogLevel = lens _chaLogLevel (\ s a -> s{_chaLogLevel = a})

-- | The unique arn of the channel.
chaARN :: Lens' ChannelSummary (Maybe Text)
chaARN = lens _chaARN (\ s a -> s{_chaARN = a})

-- | The number of currently healthy pipelines.
chaPipelinesRunningCount :: Lens' ChannelSummary (Maybe Int)
chaPipelinesRunningCount = lens _chaPipelinesRunningCount (\ s a -> s{_chaPipelinesRunningCount = a})

-- | Undocumented member.
chaInputSpecification :: Lens' ChannelSummary (Maybe InputSpecification)
chaInputSpecification = lens _chaInputSpecification (\ s a -> s{_chaInputSpecification = a})

-- | List of input attachments for channel.
chaInputAttachments :: Lens' ChannelSummary [InputAttachment]
chaInputAttachments = lens _chaInputAttachments (\ s a -> s{_chaInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
chaDestinations :: Lens' ChannelSummary [OutputDestination]
chaDestinations = lens _chaDestinations (\ s a -> s{_chaDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
chaName :: Lens' ChannelSummary (Maybe Text)
chaName = lens _chaName (\ s a -> s{_chaName = a})

-- | The unique id of the channel.
chaId :: Lens' ChannelSummary (Maybe Text)
chaId = lens _chaId (\ s a -> s{_chaId = a})

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
chaChannelClass :: Lens' ChannelSummary (Maybe ChannelClass)
chaChannelClass = lens _chaChannelClass (\ s a -> s{_chaChannelClass = a})

-- | The endpoints where outgoing connections initiate from
chaEgressEndpoints :: Lens' ChannelSummary [ChannelEgressEndpoint]
chaEgressEndpoints = lens _chaEgressEndpoints (\ s a -> s{_chaEgressEndpoints = a}) . _Default . _Coerce

-- | A collection of key-value pairs.
chaTags :: Lens' ChannelSummary (HashMap Text Text)
chaTags = lens _chaTags (\ s a -> s{_chaTags = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
chaRoleARN :: Lens' ChannelSummary (Maybe Text)
chaRoleARN = lens _chaRoleARN (\ s a -> s{_chaRoleARN = a})

instance FromJSON ChannelSummary where
        parseJSON
          = withObject "ChannelSummary"
              (\ x ->
                 ChannelSummary' <$>
                   (x .:? "state") <*> (x .:? "logLevel") <*>
                     (x .:? "arn")
                     <*> (x .:? "pipelinesRunningCount")
                     <*> (x .:? "inputSpecification")
                     <*> (x .:? "inputAttachments" .!= mempty)
                     <*> (x .:? "destinations" .!= mempty)
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "channelClass")
                     <*> (x .:? "egressEndpoints" .!= mempty)
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .:? "roleArn"))

instance Hashable ChannelSummary where

instance NFData ChannelSummary where

-- | Passthrough applies no color space conversion to the output
--
-- /See:/ 'colorSpacePassthroughSettings' smart constructor.
data ColorSpacePassthroughSettings = ColorSpacePassthroughSettings'
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'ColorSpacePassthroughSettings' with the minimum fields required to make a request.
--
colorSpacePassthroughSettings
    :: ColorSpacePassthroughSettings
colorSpacePassthroughSettings
  = ColorSpacePassthroughSettings'

instance FromJSON ColorSpacePassthroughSettings where
        parseJSON
          = withObject "ColorSpacePassthroughSettings"
              (\ x -> pure ColorSpacePassthroughSettings')

instance Hashable ColorSpacePassthroughSettings where

instance NFData ColorSpacePassthroughSettings where

instance ToJSON ColorSpacePassthroughSettings where
        toJSON = const (Object mempty)

-- | DVB Network Information Table (NIT)
--
-- /See:/ 'dvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'{_dnsRepInterval
                                      :: !(Maybe Nat),
                                      _dnsNetworkName :: !Text,
                                      _dnsNetworkId :: !Nat}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DvbNitSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnsRepInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'dnsNetworkName' - The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
--
-- * 'dnsNetworkId' - The numeric value placed in the Network Information Table (NIT).
dvbNitSettings
    :: Text -- ^ 'dnsNetworkName'
    -> Natural -- ^ 'dnsNetworkId'
    -> DvbNitSettings
dvbNitSettings pNetworkName_ pNetworkId_
  = DvbNitSettings'{_dnsRepInterval = Nothing,
                    _dnsNetworkName = pNetworkName_,
                    _dnsNetworkId = _Nat # pNetworkId_}

-- | The number of milliseconds between instances of this table in the output transport stream.
dnsRepInterval :: Lens' DvbNitSettings (Maybe Natural)
dnsRepInterval = lens _dnsRepInterval (\ s a -> s{_dnsRepInterval = a}) . mapping _Nat

-- | The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
dnsNetworkName :: Lens' DvbNitSettings Text
dnsNetworkName = lens _dnsNetworkName (\ s a -> s{_dnsNetworkName = a})

-- | The numeric value placed in the Network Information Table (NIT).
dnsNetworkId :: Lens' DvbNitSettings Natural
dnsNetworkId = lens _dnsNetworkId (\ s a -> s{_dnsNetworkId = a}) . _Nat

instance FromJSON DvbNitSettings where
        parseJSON
          = withObject "DvbNitSettings"
              (\ x ->
                 DvbNitSettings' <$>
                   (x .:? "repInterval") <*> (x .: "networkName") <*>
                     (x .: "networkId"))

instance Hashable DvbNitSettings where

instance NFData DvbNitSettings where

instance ToJSON DvbNitSettings where
        toJSON DvbNitSettings'{..}
          = object
              (catMaybes
                 [("repInterval" .=) <$> _dnsRepInterval,
                  Just ("networkName" .= _dnsNetworkName),
                  Just ("networkId" .= _dnsNetworkId)])

-- | DVB Service Description Table (SDT)
--
-- /See:/ 'dvbSdtSettings' smart constructor.
data DvbSdtSettings = DvbSdtSettings'{_dssRepInterval
                                      :: !(Maybe Nat),
                                      _dssServiceProviderName :: !(Maybe Text),
                                      _dssOutputSdt :: !(Maybe DvbSdtOutputSdt),
                                      _dssServiceName :: !(Maybe Text)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DvbSdtSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssRepInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'dssServiceProviderName' - The service provider name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
--
-- * 'dssOutputSdt' - Selects method of inserting SDT information into output stream. The sdtFollow setting copies SDT information from input stream to output stream. The sdtFollowIfPresent setting copies SDT information from input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. The sdtManual setting means user will enter the SDT information. The sdtNone setting means output stream will not contain SDT information.
--
-- * 'dssServiceName' - The service name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
dvbSdtSettings
    :: DvbSdtSettings
dvbSdtSettings
  = DvbSdtSettings'{_dssRepInterval = Nothing,
                    _dssServiceProviderName = Nothing,
                    _dssOutputSdt = Nothing, _dssServiceName = Nothing}

-- | The number of milliseconds between instances of this table in the output transport stream.
dssRepInterval :: Lens' DvbSdtSettings (Maybe Natural)
dssRepInterval = lens _dssRepInterval (\ s a -> s{_dssRepInterval = a}) . mapping _Nat

-- | The service provider name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
dssServiceProviderName :: Lens' DvbSdtSettings (Maybe Text)
dssServiceProviderName = lens _dssServiceProviderName (\ s a -> s{_dssServiceProviderName = a})

-- | Selects method of inserting SDT information into output stream. The sdtFollow setting copies SDT information from input stream to output stream. The sdtFollowIfPresent setting copies SDT information from input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. The sdtManual setting means user will enter the SDT information. The sdtNone setting means output stream will not contain SDT information.
dssOutputSdt :: Lens' DvbSdtSettings (Maybe DvbSdtOutputSdt)
dssOutputSdt = lens _dssOutputSdt (\ s a -> s{_dssOutputSdt = a})

-- | The service name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
dssServiceName :: Lens' DvbSdtSettings (Maybe Text)
dssServiceName = lens _dssServiceName (\ s a -> s{_dssServiceName = a})

instance FromJSON DvbSdtSettings where
        parseJSON
          = withObject "DvbSdtSettings"
              (\ x ->
                 DvbSdtSettings' <$>
                   (x .:? "repInterval") <*>
                     (x .:? "serviceProviderName")
                     <*> (x .:? "outputSdt")
                     <*> (x .:? "serviceName"))

instance Hashable DvbSdtSettings where

instance NFData DvbSdtSettings where

instance ToJSON DvbSdtSettings where
        toJSON DvbSdtSettings'{..}
          = object
              (catMaybes
                 [("repInterval" .=) <$> _dssRepInterval,
                  ("serviceProviderName" .=) <$>
                    _dssServiceProviderName,
                  ("outputSdt" .=) <$> _dssOutputSdt,
                  ("serviceName" .=) <$> _dssServiceName])

-- | Dvb Sub Destination Settings
--
-- /See:/ 'dvbSubDestinationSettings' smart constructor.
data DvbSubDestinationSettings = DvbSubDestinationSettings'{_dsdsBackgroundOpacity
                                                            :: !(Maybe Nat),
                                                            _dsdsFontOpacity ::
                                                            !(Maybe Nat),
                                                            _dsdsShadowYOffset
                                                            :: !(Maybe Int),
                                                            _dsdsFontResolution
                                                            :: !(Maybe Nat),
                                                            _dsdsYPosition ::
                                                            !(Maybe Nat),
                                                            _dsdsBackgroundColor
                                                            ::
                                                            !(Maybe
                                                                DvbSubDestinationBackgroundColor),
                                                            _dsdsShadowXOffset
                                                            :: !(Maybe Int),
                                                            _dsdsFontSize ::
                                                            !(Maybe Text),
                                                            _dsdsXPosition ::
                                                            !(Maybe Nat),
                                                            _dsdsAlignment ::
                                                            !(Maybe
                                                                DvbSubDestinationAlignment),
                                                            _dsdsShadowOpacity
                                                            :: !(Maybe Nat),
                                                            _dsdsTeletextGridControl
                                                            ::
                                                            !(Maybe
                                                                DvbSubDestinationTeletextGridControl),
                                                            _dsdsOutlineColor ::
                                                            !(Maybe
                                                                DvbSubDestinationOutlineColor),
                                                            _dsdsOutlineSize ::
                                                            !(Maybe Nat),
                                                            _dsdsFont ::
                                                            !(Maybe
                                                                InputLocation),
                                                            _dsdsShadowColor ::
                                                            !(Maybe
                                                                DvbSubDestinationShadowColor),
                                                            _dsdsFontColor ::
                                                            !(Maybe
                                                                DvbSubDestinationFontColor)}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DvbSubDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdsBackgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsShadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsYPosition' - Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit yPosition is provided, the caption will be positioned towards the bottom of the output.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsBackgroundColor' - Specifies the color of the rectangle behind the captions.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsShadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontSize' - When set to auto fontSize will scale depending on the size of the output.  Giving a positive integer will specify the exact font size in points.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsXPosition' - Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit xPosition is provided, the horizontal caption position will be determined by the alignment parameter.  This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsAlignment' - If no explicit xPosition or yPosition is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. Selecting "smart" justification will left-justify live subtitles and center-justify pre-recorded subtitles.  This option is not valid for source captions that are STL or 608/embedded.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsShadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsTeletextGridControl' - Controls whether a fixed grid size will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
--
-- * 'dsdsOutlineColor' - Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsOutlineSize' - Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFont' - External font file used for caption burn-in. File extension must be 'ttf' or 'tte'.  Although the user can select output fonts for many different types of input captions, embedded, STL and teletext sources use a strict grid system. Using external fonts with these caption sources could cause unexpected display of proportional fonts.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsShadowColor' - Specifies the color of the shadow cast by the captions.  All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontColor' - Specifies the color of the burned-in captions.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings
    :: DvbSubDestinationSettings
dvbSubDestinationSettings
  = DvbSubDestinationSettings'{_dsdsBackgroundOpacity =
                                 Nothing,
                               _dsdsFontOpacity = Nothing,
                               _dsdsShadowYOffset = Nothing,
                               _dsdsFontResolution = Nothing,
                               _dsdsYPosition = Nothing,
                               _dsdsBackgroundColor = Nothing,
                               _dsdsShadowXOffset = Nothing,
                               _dsdsFontSize = Nothing,
                               _dsdsXPosition = Nothing,
                               _dsdsAlignment = Nothing,
                               _dsdsShadowOpacity = Nothing,
                               _dsdsTeletextGridControl = Nothing,
                               _dsdsOutlineColor = Nothing,
                               _dsdsOutlineSize = Nothing, _dsdsFont = Nothing,
                               _dsdsShadowColor = Nothing,
                               _dsdsFontColor = Nothing}

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
dsdsBackgroundOpacity :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsBackgroundOpacity = lens _dsdsBackgroundOpacity (\ s a -> s{_dsdsBackgroundOpacity = a}) . mapping _Nat

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent.  All burn-in and DVB-Sub font settings must match.
dsdsFontOpacity :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsFontOpacity = lens _dsdsFontOpacity (\ s a -> s{_dsdsFontOpacity = a}) . mapping _Nat

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text.  All burn-in and DVB-Sub font settings must match.
dsdsShadowYOffset :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsShadowYOffset = lens _dsdsShadowYOffset (\ s a -> s{_dsdsShadowYOffset = a})

-- | Font resolution in DPI (dots per inch); default is 96 dpi.  All burn-in and DVB-Sub font settings must match.
dsdsFontResolution :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsFontResolution = lens _dsdsFontResolution (\ s a -> s{_dsdsFontResolution = a}) . mapping _Nat

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit yPosition is provided, the caption will be positioned towards the bottom of the output.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
dsdsYPosition :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsYPosition = lens _dsdsYPosition (\ s a -> s{_dsdsYPosition = a}) . mapping _Nat

-- | Specifies the color of the rectangle behind the captions.  All burn-in and DVB-Sub font settings must match.
dsdsBackgroundColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubDestinationBackgroundColor)
dsdsBackgroundColor = lens _dsdsBackgroundColor (\ s a -> s{_dsdsBackgroundColor = a})

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left.  All burn-in and DVB-Sub font settings must match.
dsdsShadowXOffset :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsShadowXOffset = lens _dsdsShadowXOffset (\ s a -> s{_dsdsShadowXOffset = a})

-- | When set to auto fontSize will scale depending on the size of the output.  Giving a positive integer will specify the exact font size in points.  All burn-in and DVB-Sub font settings must match.
dsdsFontSize :: Lens' DvbSubDestinationSettings (Maybe Text)
dsdsFontSize = lens _dsdsFontSize (\ s a -> s{_dsdsFontSize = a})

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit xPosition is provided, the horizontal caption position will be determined by the alignment parameter.  This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
dsdsXPosition :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsXPosition = lens _dsdsXPosition (\ s a -> s{_dsdsXPosition = a}) . mapping _Nat

-- | If no explicit xPosition or yPosition is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. Selecting "smart" justification will left-justify live subtitles and center-justify pre-recorded subtitles.  This option is not valid for source captions that are STL or 608/embedded.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
dsdsAlignment :: Lens' DvbSubDestinationSettings (Maybe DvbSubDestinationAlignment)
dsdsAlignment = lens _dsdsAlignment (\ s a -> s{_dsdsAlignment = a})

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent).  All burn-in and DVB-Sub font settings must match.
dsdsShadowOpacity :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsShadowOpacity = lens _dsdsShadowOpacity (\ s a -> s{_dsdsShadowOpacity = a}) . mapping _Nat

-- | Controls whether a fixed grid size will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
dsdsTeletextGridControl :: Lens' DvbSubDestinationSettings (Maybe DvbSubDestinationTeletextGridControl)
dsdsTeletextGridControl = lens _dsdsTeletextGridControl (\ s a -> s{_dsdsTeletextGridControl = a})

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsOutlineColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubDestinationOutlineColor)
dsdsOutlineColor = lens _dsdsOutlineColor (\ s a -> s{_dsdsOutlineColor = a})

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsOutlineSize :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsOutlineSize = lens _dsdsOutlineSize (\ s a -> s{_dsdsOutlineSize = a}) . mapping _Nat

-- | External font file used for caption burn-in. File extension must be 'ttf' or 'tte'.  Although the user can select output fonts for many different types of input captions, embedded, STL and teletext sources use a strict grid system. Using external fonts with these caption sources could cause unexpected display of proportional fonts.  All burn-in and DVB-Sub font settings must match.
dsdsFont :: Lens' DvbSubDestinationSettings (Maybe InputLocation)
dsdsFont = lens _dsdsFont (\ s a -> s{_dsdsFont = a})

-- | Specifies the color of the shadow cast by the captions.  All burn-in and DVB-Sub font settings must match.
dsdsShadowColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubDestinationShadowColor)
dsdsShadowColor = lens _dsdsShadowColor (\ s a -> s{_dsdsShadowColor = a})

-- | Specifies the color of the burned-in captions.  This option is not valid for source captions that are STL, 608/embedded or teletext.  These source settings are already pre-defined by the caption stream.  All burn-in and DVB-Sub font settings must match.
dsdsFontColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubDestinationFontColor)
dsdsFontColor = lens _dsdsFontColor (\ s a -> s{_dsdsFontColor = a})

instance FromJSON DvbSubDestinationSettings where
        parseJSON
          = withObject "DvbSubDestinationSettings"
              (\ x ->
                 DvbSubDestinationSettings' <$>
                   (x .:? "backgroundOpacity") <*> (x .:? "fontOpacity")
                     <*> (x .:? "shadowYOffset")
                     <*> (x .:? "fontResolution")
                     <*> (x .:? "yPosition")
                     <*> (x .:? "backgroundColor")
                     <*> (x .:? "shadowXOffset")
                     <*> (x .:? "fontSize")
                     <*> (x .:? "xPosition")
                     <*> (x .:? "alignment")
                     <*> (x .:? "shadowOpacity")
                     <*> (x .:? "teletextGridControl")
                     <*> (x .:? "outlineColor")
                     <*> (x .:? "outlineSize")
                     <*> (x .:? "font")
                     <*> (x .:? "shadowColor")
                     <*> (x .:? "fontColor"))

instance Hashable DvbSubDestinationSettings where

instance NFData DvbSubDestinationSettings where

instance ToJSON DvbSubDestinationSettings where
        toJSON DvbSubDestinationSettings'{..}
          = object
              (catMaybes
                 [("backgroundOpacity" .=) <$> _dsdsBackgroundOpacity,
                  ("fontOpacity" .=) <$> _dsdsFontOpacity,
                  ("shadowYOffset" .=) <$> _dsdsShadowYOffset,
                  ("fontResolution" .=) <$> _dsdsFontResolution,
                  ("yPosition" .=) <$> _dsdsYPosition,
                  ("backgroundColor" .=) <$> _dsdsBackgroundColor,
                  ("shadowXOffset" .=) <$> _dsdsShadowXOffset,
                  ("fontSize" .=) <$> _dsdsFontSize,
                  ("xPosition" .=) <$> _dsdsXPosition,
                  ("alignment" .=) <$> _dsdsAlignment,
                  ("shadowOpacity" .=) <$> _dsdsShadowOpacity,
                  ("teletextGridControl" .=) <$>
                    _dsdsTeletextGridControl,
                  ("outlineColor" .=) <$> _dsdsOutlineColor,
                  ("outlineSize" .=) <$> _dsdsOutlineSize,
                  ("font" .=) <$> _dsdsFont,
                  ("shadowColor" .=) <$> _dsdsShadowColor,
                  ("fontColor" .=) <$> _dsdsFontColor])

-- | Dvb Sub Source Settings
--
-- /See:/ 'dvbSubSourceSettings' smart constructor.
newtype DvbSubSourceSettings = DvbSubSourceSettings'{_dsssPid
                                                     :: Maybe Nat}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'DvbSubSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsssPid' - When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed through, regardless of selectors.
dvbSubSourceSettings
    :: DvbSubSourceSettings
dvbSubSourceSettings
  = DvbSubSourceSettings'{_dsssPid = Nothing}

-- | When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed through, regardless of selectors.
dsssPid :: Lens' DvbSubSourceSettings (Maybe Natural)
dsssPid = lens _dsssPid (\ s a -> s{_dsssPid = a}) . mapping _Nat

instance FromJSON DvbSubSourceSettings where
        parseJSON
          = withObject "DvbSubSourceSettings"
              (\ x -> DvbSubSourceSettings' <$> (x .:? "pid"))

instance Hashable DvbSubSourceSettings where

instance NFData DvbSubSourceSettings where

instance ToJSON DvbSubSourceSettings where
        toJSON DvbSubSourceSettings'{..}
          = object (catMaybes [("pid" .=) <$> _dsssPid])

-- | DVB Time and Date Table (SDT)
--
-- /See:/ 'dvbTdtSettings' smart constructor.
newtype DvbTdtSettings = DvbTdtSettings'{_dtsRepInterval
                                         :: Maybe Nat}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DvbTdtSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsRepInterval' - The number of milliseconds between instances of this table in the output transport stream.
dvbTdtSettings
    :: DvbTdtSettings
dvbTdtSettings
  = DvbTdtSettings'{_dtsRepInterval = Nothing}

-- | The number of milliseconds between instances of this table in the output transport stream.
dtsRepInterval :: Lens' DvbTdtSettings (Maybe Natural)
dtsRepInterval = lens _dtsRepInterval (\ s a -> s{_dtsRepInterval = a}) . mapping _Nat

instance FromJSON DvbTdtSettings where
        parseJSON
          = withObject "DvbTdtSettings"
              (\ x -> DvbTdtSettings' <$> (x .:? "repInterval"))

instance Hashable DvbTdtSettings where

instance NFData DvbTdtSettings where

instance ToJSON DvbTdtSettings where
        toJSON DvbTdtSettings'{..}
          = object
              (catMaybes [("repInterval" .=) <$> _dtsRepInterval])

-- | Eac3 Settings
--
-- /See:/ 'eac3Settings' smart constructor.
data Eac3Settings = Eac3Settings'{_esStereoDownmix ::
                                  !(Maybe Eac3StereoDownmix),
                                  _esLoRoCenterMixLevel :: !(Maybe Double),
                                  _esLtRtCenterMixLevel :: !(Maybe Double),
                                  _esLfeFilter :: !(Maybe Eac3LfeFilter),
                                  _esLtRtSurroundMixLevel :: !(Maybe Double),
                                  _esMetadataControl ::
                                  !(Maybe Eac3MetadataControl),
                                  _esLoRoSurroundMixLevel :: !(Maybe Double),
                                  _esSurroundMode :: !(Maybe Eac3SurroundMode),
                                  _esAttenuationControl ::
                                  !(Maybe Eac3AttenuationControl),
                                  _esPassthroughControl ::
                                  !(Maybe Eac3PassthroughControl),
                                  _esBitstreamMode ::
                                  !(Maybe Eac3BitstreamMode),
                                  _esLfeControl :: !(Maybe Eac3LfeControl),
                                  _esCodingMode :: !(Maybe Eac3CodingMode),
                                  _esDrcLine :: !(Maybe Eac3DrcLine),
                                  _esDrcRf :: !(Maybe Eac3DrcRf),
                                  _esDcFilter :: !(Maybe Eac3DcFilter),
                                  _esBitrate :: !(Maybe Double),
                                  _esPhaseControl :: !(Maybe Eac3PhaseControl),
                                  _esSurroundExMode ::
                                  !(Maybe Eac3SurroundExMode),
                                  _esDialnorm :: !(Maybe Nat)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Eac3Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esStereoDownmix' - Stereo downmix preference. Only used for 3/2 coding mode.
--
-- * 'esLoRoCenterMixLevel' - Left only/Right only center mix level. Only used for 3/2 coding mode.
--
-- * 'esLtRtCenterMixLevel' - Left total/Right total center mix level. Only used for 3/2 coding mode.
--
-- * 'esLfeFilter' - When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with codingMode32 coding mode.
--
-- * 'esLtRtSurroundMixLevel' - Left total/Right total surround mix level. Only used for 3/2 coding mode.
--
-- * 'esMetadataControl' - When set to followInput, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- * 'esLoRoSurroundMixLevel' - Left only/Right only surround mix level. Only used for 3/2 coding mode.
--
-- * 'esSurroundMode' - When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
--
-- * 'esAttenuationControl' - When set to attenuate3Db, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
--
-- * 'esPassthroughControl' - When set to whenPossible, input DD+ audio will be passed through if it is present on the input. This detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
--
-- * 'esBitstreamMode' - Specifies the bitstream mode (bsmod) for the emitted E-AC-3 stream. See ATSC A/52-2012 (Annex E) for background on these values.
--
-- * 'esLfeControl' - When encoding 3/2 audio, setting to lfe enables the LFE channel
--
-- * 'esCodingMode' - Dolby Digital Plus coding mode. Determines number of channels.
--
-- * 'esDrcLine' - Sets the Dolby dynamic range compression profile.
--
-- * 'esDrcRf' - Sets the profile for heavy Dolby dynamic range compression, ensures that the instantaneous signal peaks do not exceed specified levels.
--
-- * 'esDcFilter' - When set to enabled, activates a DC highpass filter for all input channels.
--
-- * 'esBitrate' - Average bitrate in bits/second. Valid bitrates depend on the coding mode.
--
-- * 'esPhaseControl' - When set to shift90Degrees, applies a 90-degree phase shift to the surround channels. Only used for 3/2 coding mode.
--
-- * 'esSurroundExMode' - When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
--
-- * 'esDialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
eac3Settings
    :: Eac3Settings
eac3Settings
  = Eac3Settings'{_esStereoDownmix = Nothing,
                  _esLoRoCenterMixLevel = Nothing,
                  _esLtRtCenterMixLevel = Nothing,
                  _esLfeFilter = Nothing,
                  _esLtRtSurroundMixLevel = Nothing,
                  _esMetadataControl = Nothing,
                  _esLoRoSurroundMixLevel = Nothing,
                  _esSurroundMode = Nothing,
                  _esAttenuationControl = Nothing,
                  _esPassthroughControl = Nothing,
                  _esBitstreamMode = Nothing, _esLfeControl = Nothing,
                  _esCodingMode = Nothing, _esDrcLine = Nothing,
                  _esDrcRf = Nothing, _esDcFilter = Nothing,
                  _esBitrate = Nothing, _esPhaseControl = Nothing,
                  _esSurroundExMode = Nothing, _esDialnorm = Nothing}

-- | Stereo downmix preference. Only used for 3/2 coding mode.
esStereoDownmix :: Lens' Eac3Settings (Maybe Eac3StereoDownmix)
esStereoDownmix = lens _esStereoDownmix (\ s a -> s{_esStereoDownmix = a})

-- | Left only/Right only center mix level. Only used for 3/2 coding mode.
esLoRoCenterMixLevel :: Lens' Eac3Settings (Maybe Double)
esLoRoCenterMixLevel = lens _esLoRoCenterMixLevel (\ s a -> s{_esLoRoCenterMixLevel = a})

-- | Left total/Right total center mix level. Only used for 3/2 coding mode.
esLtRtCenterMixLevel :: Lens' Eac3Settings (Maybe Double)
esLtRtCenterMixLevel = lens _esLtRtCenterMixLevel (\ s a -> s{_esLtRtCenterMixLevel = a})

-- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with codingMode32 coding mode.
esLfeFilter :: Lens' Eac3Settings (Maybe Eac3LfeFilter)
esLfeFilter = lens _esLfeFilter (\ s a -> s{_esLfeFilter = a})

-- | Left total/Right total surround mix level. Only used for 3/2 coding mode.
esLtRtSurroundMixLevel :: Lens' Eac3Settings (Maybe Double)
esLtRtSurroundMixLevel = lens _esLtRtSurroundMixLevel (\ s a -> s{_esLtRtSurroundMixLevel = a})

-- | When set to followInput, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
esMetadataControl :: Lens' Eac3Settings (Maybe Eac3MetadataControl)
esMetadataControl = lens _esMetadataControl (\ s a -> s{_esMetadataControl = a})

-- | Left only/Right only surround mix level. Only used for 3/2 coding mode.
esLoRoSurroundMixLevel :: Lens' Eac3Settings (Maybe Double)
esLoRoSurroundMixLevel = lens _esLoRoSurroundMixLevel (\ s a -> s{_esLoRoSurroundMixLevel = a})

-- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
esSurroundMode :: Lens' Eac3Settings (Maybe Eac3SurroundMode)
esSurroundMode = lens _esSurroundMode (\ s a -> s{_esSurroundMode = a})

-- | When set to attenuate3Db, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
esAttenuationControl :: Lens' Eac3Settings (Maybe Eac3AttenuationControl)
esAttenuationControl = lens _esAttenuationControl (\ s a -> s{_esAttenuationControl = a})

-- | When set to whenPossible, input DD+ audio will be passed through if it is present on the input. This detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
esPassthroughControl :: Lens' Eac3Settings (Maybe Eac3PassthroughControl)
esPassthroughControl = lens _esPassthroughControl (\ s a -> s{_esPassthroughControl = a})

-- | Specifies the bitstream mode (bsmod) for the emitted E-AC-3 stream. See ATSC A/52-2012 (Annex E) for background on these values.
esBitstreamMode :: Lens' Eac3Settings (Maybe Eac3BitstreamMode)
esBitstreamMode = lens _esBitstreamMode (\ s a -> s{_esBitstreamMode = a})

-- | When encoding 3/2 audio, setting to lfe enables the LFE channel
esLfeControl :: Lens' Eac3Settings (Maybe Eac3LfeControl)
esLfeControl = lens _esLfeControl (\ s a -> s{_esLfeControl = a})

-- | Dolby Digital Plus coding mode. Determines number of channels.
esCodingMode :: Lens' Eac3Settings (Maybe Eac3CodingMode)
esCodingMode = lens _esCodingMode (\ s a -> s{_esCodingMode = a})

-- | Sets the Dolby dynamic range compression profile.
esDrcLine :: Lens' Eac3Settings (Maybe Eac3DrcLine)
esDrcLine = lens _esDrcLine (\ s a -> s{_esDrcLine = a})

-- | Sets the profile for heavy Dolby dynamic range compression, ensures that the instantaneous signal peaks do not exceed specified levels.
esDrcRf :: Lens' Eac3Settings (Maybe Eac3DrcRf)
esDrcRf = lens _esDrcRf (\ s a -> s{_esDrcRf = a})

-- | When set to enabled, activates a DC highpass filter for all input channels.
esDcFilter :: Lens' Eac3Settings (Maybe Eac3DcFilter)
esDcFilter = lens _esDcFilter (\ s a -> s{_esDcFilter = a})

-- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
esBitrate :: Lens' Eac3Settings (Maybe Double)
esBitrate = lens _esBitrate (\ s a -> s{_esBitrate = a})

-- | When set to shift90Degrees, applies a 90-degree phase shift to the surround channels. Only used for 3/2 coding mode.
esPhaseControl :: Lens' Eac3Settings (Maybe Eac3PhaseControl)
esPhaseControl = lens _esPhaseControl (\ s a -> s{_esPhaseControl = a})

-- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
esSurroundExMode :: Lens' Eac3Settings (Maybe Eac3SurroundExMode)
esSurroundExMode = lens _esSurroundExMode (\ s a -> s{_esSurroundExMode = a})

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
esDialnorm :: Lens' Eac3Settings (Maybe Natural)
esDialnorm = lens _esDialnorm (\ s a -> s{_esDialnorm = a}) . mapping _Nat

instance FromJSON Eac3Settings where
        parseJSON
          = withObject "Eac3Settings"
              (\ x ->
                 Eac3Settings' <$>
                   (x .:? "stereoDownmix") <*>
                     (x .:? "loRoCenterMixLevel")
                     <*> (x .:? "ltRtCenterMixLevel")
                     <*> (x .:? "lfeFilter")
                     <*> (x .:? "ltRtSurroundMixLevel")
                     <*> (x .:? "metadataControl")
                     <*> (x .:? "loRoSurroundMixLevel")
                     <*> (x .:? "surroundMode")
                     <*> (x .:? "attenuationControl")
                     <*> (x .:? "passthroughControl")
                     <*> (x .:? "bitstreamMode")
                     <*> (x .:? "lfeControl")
                     <*> (x .:? "codingMode")
                     <*> (x .:? "drcLine")
                     <*> (x .:? "drcRf")
                     <*> (x .:? "dcFilter")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "phaseControl")
                     <*> (x .:? "surroundExMode")
                     <*> (x .:? "dialnorm"))

instance Hashable Eac3Settings where

instance NFData Eac3Settings where

instance ToJSON Eac3Settings where
        toJSON Eac3Settings'{..}
          = object
              (catMaybes
                 [("stereoDownmix" .=) <$> _esStereoDownmix,
                  ("loRoCenterMixLevel" .=) <$> _esLoRoCenterMixLevel,
                  ("ltRtCenterMixLevel" .=) <$> _esLtRtCenterMixLevel,
                  ("lfeFilter" .=) <$> _esLfeFilter,
                  ("ltRtSurroundMixLevel" .=) <$>
                    _esLtRtSurroundMixLevel,
                  ("metadataControl" .=) <$> _esMetadataControl,
                  ("loRoSurroundMixLevel" .=) <$>
                    _esLoRoSurroundMixLevel,
                  ("surroundMode" .=) <$> _esSurroundMode,
                  ("attenuationControl" .=) <$> _esAttenuationControl,
                  ("passthroughControl" .=) <$> _esPassthroughControl,
                  ("bitstreamMode" .=) <$> _esBitstreamMode,
                  ("lfeControl" .=) <$> _esLfeControl,
                  ("codingMode" .=) <$> _esCodingMode,
                  ("drcLine" .=) <$> _esDrcLine,
                  ("drcRf" .=) <$> _esDrcRf,
                  ("dcFilter" .=) <$> _esDcFilter,
                  ("bitrate" .=) <$> _esBitrate,
                  ("phaseControl" .=) <$> _esPhaseControl,
                  ("surroundExMode" .=) <$> _esSurroundExMode,
                  ("dialnorm" .=) <$> _esDialnorm])

-- | Embedded Destination Settings
--
-- /See:/ 'embeddedDestinationSettings' smart constructor.
data EmbeddedDestinationSettings = EmbeddedDestinationSettings'
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'EmbeddedDestinationSettings' with the minimum fields required to make a request.
--
embeddedDestinationSettings
    :: EmbeddedDestinationSettings
embeddedDestinationSettings
  = EmbeddedDestinationSettings'

instance FromJSON EmbeddedDestinationSettings where
        parseJSON
          = withObject "EmbeddedDestinationSettings"
              (\ x -> pure EmbeddedDestinationSettings')

instance Hashable EmbeddedDestinationSettings where

instance NFData EmbeddedDestinationSettings where

instance ToJSON EmbeddedDestinationSettings where
        toJSON = const (Object mempty)

-- | Embedded Plus Scte20 Destination Settings
--
-- /See:/ 'embeddedPlusScte20DestinationSettings' smart constructor.
data EmbeddedPlusScte20DestinationSettings = EmbeddedPlusScte20DestinationSettings'
                                               deriving (Eq, Read, Show, Data,
                                                         Typeable, Generic)

-- | Creates a value of 'EmbeddedPlusScte20DestinationSettings' with the minimum fields required to make a request.
--
embeddedPlusScte20DestinationSettings
    :: EmbeddedPlusScte20DestinationSettings
embeddedPlusScte20DestinationSettings
  = EmbeddedPlusScte20DestinationSettings'

instance FromJSON
           EmbeddedPlusScte20DestinationSettings
         where
        parseJSON
          = withObject "EmbeddedPlusScte20DestinationSettings"
              (\ x -> pure EmbeddedPlusScte20DestinationSettings')

instance Hashable
           EmbeddedPlusScte20DestinationSettings
         where

instance NFData EmbeddedPlusScte20DestinationSettings
         where

instance ToJSON EmbeddedPlusScte20DestinationSettings
         where
        toJSON = const (Object mempty)

-- | Embedded Source Settings
--
-- /See:/ 'embeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'{_essConvert608To708
                                                      ::
                                                      !(Maybe
                                                          EmbeddedConvert608To708),
                                                      _essScte20Detection ::
                                                      !(Maybe
                                                          EmbeddedScte20Detection),
                                                      _essSource608TrackNumber
                                                      :: !(Maybe Nat),
                                                      _essSource608ChannelNumber
                                                      :: !(Maybe Nat)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'EmbeddedSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'essConvert608To708' - If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
--
-- * 'essScte20Detection' - Set to "auto" to handle streams with intermittent and/or non-aligned SCTE-20 and Embedded captions.
--
-- * 'essSource608TrackNumber' - This field is unused and deprecated.
--
-- * 'essSource608ChannelNumber' - Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
embeddedSourceSettings
    :: EmbeddedSourceSettings
embeddedSourceSettings
  = EmbeddedSourceSettings'{_essConvert608To708 =
                              Nothing,
                            _essScte20Detection = Nothing,
                            _essSource608TrackNumber = Nothing,
                            _essSource608ChannelNumber = Nothing}

-- | If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
essConvert608To708 :: Lens' EmbeddedSourceSettings (Maybe EmbeddedConvert608To708)
essConvert608To708 = lens _essConvert608To708 (\ s a -> s{_essConvert608To708 = a})

-- | Set to "auto" to handle streams with intermittent and/or non-aligned SCTE-20 and Embedded captions.
essScte20Detection :: Lens' EmbeddedSourceSettings (Maybe EmbeddedScte20Detection)
essScte20Detection = lens _essScte20Detection (\ s a -> s{_essScte20Detection = a})

-- | This field is unused and deprecated.
essSource608TrackNumber :: Lens' EmbeddedSourceSettings (Maybe Natural)
essSource608TrackNumber = lens _essSource608TrackNumber (\ s a -> s{_essSource608TrackNumber = a}) . mapping _Nat

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
essSource608ChannelNumber :: Lens' EmbeddedSourceSettings (Maybe Natural)
essSource608ChannelNumber = lens _essSource608ChannelNumber (\ s a -> s{_essSource608ChannelNumber = a}) . mapping _Nat

instance FromJSON EmbeddedSourceSettings where
        parseJSON
          = withObject "EmbeddedSourceSettings"
              (\ x ->
                 EmbeddedSourceSettings' <$>
                   (x .:? "convert608To708") <*>
                     (x .:? "scte20Detection")
                     <*> (x .:? "source608TrackNumber")
                     <*> (x .:? "source608ChannelNumber"))

instance Hashable EmbeddedSourceSettings where

instance NFData EmbeddedSourceSettings where

instance ToJSON EmbeddedSourceSettings where
        toJSON EmbeddedSourceSettings'{..}
          = object
              (catMaybes
                 [("convert608To708" .=) <$> _essConvert608To708,
                  ("scte20Detection" .=) <$> _essScte20Detection,
                  ("source608TrackNumber" .=) <$>
                    _essSource608TrackNumber,
                  ("source608ChannelNumber" .=) <$>
                    _essSource608ChannelNumber])

-- | Encoder Settings
--
-- /See:/ 'encoderSettings' smart constructor.
data EncoderSettings = EncoderSettings'{_esCaptionDescriptions
                                        :: !(Maybe [CaptionDescription]),
                                        _esAvailConfiguration ::
                                        !(Maybe AvailConfiguration),
                                        _esFeatureActivations ::
                                        !(Maybe FeatureActivations),
                                        _esNielsenConfiguration ::
                                        !(Maybe NielsenConfiguration),
                                        _esAvailBlanking ::
                                        !(Maybe AvailBlanking),
                                        _esGlobalConfiguration ::
                                        !(Maybe GlobalConfiguration),
                                        _esBlackoutSlate ::
                                        !(Maybe BlackoutSlate),
                                        _esVideoDescriptions ::
                                        ![VideoDescription],
                                        _esAudioDescriptions ::
                                        ![AudioDescription],
                                        _esOutputGroups :: ![OutputGroup],
                                        _esTimecodeConfig :: !TimecodeConfig}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncoderSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esCaptionDescriptions' - Settings for caption decriptions
--
-- * 'esAvailConfiguration' - Event-wide configuration settings for ad avail insertion.
--
-- * 'esFeatureActivations' - Feature Activations
--
-- * 'esNielsenConfiguration' - Nielsen configuration settings.
--
-- * 'esAvailBlanking' - Settings for ad avail blanking.
--
-- * 'esGlobalConfiguration' - Configuration settings that apply to the event as a whole.
--
-- * 'esBlackoutSlate' - Settings for blackout slate.
--
-- * 'esVideoDescriptions' - Undocumented member.
--
-- * 'esAudioDescriptions' - Undocumented member.
--
-- * 'esOutputGroups' - Undocumented member.
--
-- * 'esTimecodeConfig' - Contains settings used to acquire and adjust timecode information from inputs.
encoderSettings
    :: TimecodeConfig -- ^ 'esTimecodeConfig'
    -> EncoderSettings
encoderSettings pTimecodeConfig_
  = EncoderSettings'{_esCaptionDescriptions = Nothing,
                     _esAvailConfiguration = Nothing,
                     _esFeatureActivations = Nothing,
                     _esNielsenConfiguration = Nothing,
                     _esAvailBlanking = Nothing,
                     _esGlobalConfiguration = Nothing,
                     _esBlackoutSlate = Nothing,
                     _esVideoDescriptions = mempty,
                     _esAudioDescriptions = mempty,
                     _esOutputGroups = mempty,
                     _esTimecodeConfig = pTimecodeConfig_}

-- | Settings for caption decriptions
esCaptionDescriptions :: Lens' EncoderSettings [CaptionDescription]
esCaptionDescriptions = lens _esCaptionDescriptions (\ s a -> s{_esCaptionDescriptions = a}) . _Default . _Coerce

-- | Event-wide configuration settings for ad avail insertion.
esAvailConfiguration :: Lens' EncoderSettings (Maybe AvailConfiguration)
esAvailConfiguration = lens _esAvailConfiguration (\ s a -> s{_esAvailConfiguration = a})

-- | Feature Activations
esFeatureActivations :: Lens' EncoderSettings (Maybe FeatureActivations)
esFeatureActivations = lens _esFeatureActivations (\ s a -> s{_esFeatureActivations = a})

-- | Nielsen configuration settings.
esNielsenConfiguration :: Lens' EncoderSettings (Maybe NielsenConfiguration)
esNielsenConfiguration = lens _esNielsenConfiguration (\ s a -> s{_esNielsenConfiguration = a})

-- | Settings for ad avail blanking.
esAvailBlanking :: Lens' EncoderSettings (Maybe AvailBlanking)
esAvailBlanking = lens _esAvailBlanking (\ s a -> s{_esAvailBlanking = a})

-- | Configuration settings that apply to the event as a whole.
esGlobalConfiguration :: Lens' EncoderSettings (Maybe GlobalConfiguration)
esGlobalConfiguration = lens _esGlobalConfiguration (\ s a -> s{_esGlobalConfiguration = a})

-- | Settings for blackout slate.
esBlackoutSlate :: Lens' EncoderSettings (Maybe BlackoutSlate)
esBlackoutSlate = lens _esBlackoutSlate (\ s a -> s{_esBlackoutSlate = a})

-- | Undocumented member.
esVideoDescriptions :: Lens' EncoderSettings [VideoDescription]
esVideoDescriptions = lens _esVideoDescriptions (\ s a -> s{_esVideoDescriptions = a}) . _Coerce

-- | Undocumented member.
esAudioDescriptions :: Lens' EncoderSettings [AudioDescription]
esAudioDescriptions = lens _esAudioDescriptions (\ s a -> s{_esAudioDescriptions = a}) . _Coerce

-- | Undocumented member.
esOutputGroups :: Lens' EncoderSettings [OutputGroup]
esOutputGroups = lens _esOutputGroups (\ s a -> s{_esOutputGroups = a}) . _Coerce

-- | Contains settings used to acquire and adjust timecode information from inputs.
esTimecodeConfig :: Lens' EncoderSettings TimecodeConfig
esTimecodeConfig = lens _esTimecodeConfig (\ s a -> s{_esTimecodeConfig = a})

instance FromJSON EncoderSettings where
        parseJSON
          = withObject "EncoderSettings"
              (\ x ->
                 EncoderSettings' <$>
                   (x .:? "captionDescriptions" .!= mempty) <*>
                     (x .:? "availConfiguration")
                     <*> (x .:? "featureActivations")
                     <*> (x .:? "nielsenConfiguration")
                     <*> (x .:? "availBlanking")
                     <*> (x .:? "globalConfiguration")
                     <*> (x .:? "blackoutSlate")
                     <*> (x .:? "videoDescriptions" .!= mempty)
                     <*> (x .:? "audioDescriptions" .!= mempty)
                     <*> (x .:? "outputGroups" .!= mempty)
                     <*> (x .: "timecodeConfig"))

instance Hashable EncoderSettings where

instance NFData EncoderSettings where

instance ToJSON EncoderSettings where
        toJSON EncoderSettings'{..}
          = object
              (catMaybes
                 [("captionDescriptions" .=) <$>
                    _esCaptionDescriptions,
                  ("availConfiguration" .=) <$> _esAvailConfiguration,
                  ("featureActivations" .=) <$> _esFeatureActivations,
                  ("nielsenConfiguration" .=) <$>
                    _esNielsenConfiguration,
                  ("availBlanking" .=) <$> _esAvailBlanking,
                  ("globalConfiguration" .=) <$>
                    _esGlobalConfiguration,
                  ("blackoutSlate" .=) <$> _esBlackoutSlate,
                  Just ("videoDescriptions" .= _esVideoDescriptions),
                  Just ("audioDescriptions" .= _esAudioDescriptions),
                  Just ("outputGroups" .= _esOutputGroups),
                  Just ("timecodeConfig" .= _esTimecodeConfig)])

-- | Feature Activations
--
-- /See:/ 'featureActivations' smart constructor.
newtype FeatureActivations = FeatureActivations'{_faInputPrepareScheduleActions
                                                 ::
                                                 Maybe
                                                   FeatureActivationsInputPrepareScheduleActions}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'FeatureActivations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faInputPrepareScheduleActions' - Enables the Input Prepare feature. You can create Input Prepare actions in the schedule only if this feature is enabled. If you disable the feature on an existing schedule, make sure that you first delete all input prepare actions from the schedule.
featureActivations
    :: FeatureActivations
featureActivations
  = FeatureActivations'{_faInputPrepareScheduleActions
                          = Nothing}

-- | Enables the Input Prepare feature. You can create Input Prepare actions in the schedule only if this feature is enabled. If you disable the feature on an existing schedule, make sure that you first delete all input prepare actions from the schedule.
faInputPrepareScheduleActions :: Lens' FeatureActivations (Maybe FeatureActivationsInputPrepareScheduleActions)
faInputPrepareScheduleActions = lens _faInputPrepareScheduleActions (\ s a -> s{_faInputPrepareScheduleActions = a})

instance FromJSON FeatureActivations where
        parseJSON
          = withObject "FeatureActivations"
              (\ x ->
                 FeatureActivations' <$>
                   (x .:? "inputPrepareScheduleActions"))

instance Hashable FeatureActivations where

instance NFData FeatureActivations where

instance ToJSON FeatureActivations where
        toJSON FeatureActivations'{..}
          = object
              (catMaybes
                 [("inputPrepareScheduleActions" .=) <$>
                    _faInputPrepareScheduleActions])

-- | Fec Output Settings
--
-- /See:/ 'fecOutputSettings' smart constructor.
data FecOutputSettings = FecOutputSettings'{_fosRowLength
                                            :: !(Maybe Nat),
                                            _fosIncludeFec ::
                                            !(Maybe FecOutputIncludeFec),
                                            _fosColumnDepth :: !(Maybe Nat)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FecOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fosRowLength' - Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.  Must be between 1 and 20, inclusive. If only Column FEC is used, then larger values increase robustness.  If Row FEC is used, then this is the number of transport stream packets per row error correction packet, and the value must be between 4 and 20, inclusive, if includeFec is columnAndRow. If includeFec is column, this value must be 1 to 20, inclusive.
--
-- * 'fosIncludeFec' - Enables column only or column and row based FEC
--
-- * 'fosColumnDepth' - Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.  The number of transport stream packets per column error correction packet. Must be between 4 and 20, inclusive.
fecOutputSettings
    :: FecOutputSettings
fecOutputSettings
  = FecOutputSettings'{_fosRowLength = Nothing,
                       _fosIncludeFec = Nothing, _fosColumnDepth = Nothing}

-- | Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.  Must be between 1 and 20, inclusive. If only Column FEC is used, then larger values increase robustness.  If Row FEC is used, then this is the number of transport stream packets per row error correction packet, and the value must be between 4 and 20, inclusive, if includeFec is columnAndRow. If includeFec is column, this value must be 1 to 20, inclusive.
fosRowLength :: Lens' FecOutputSettings (Maybe Natural)
fosRowLength = lens _fosRowLength (\ s a -> s{_fosRowLength = a}) . mapping _Nat

-- | Enables column only or column and row based FEC
fosIncludeFec :: Lens' FecOutputSettings (Maybe FecOutputIncludeFec)
fosIncludeFec = lens _fosIncludeFec (\ s a -> s{_fosIncludeFec = a})

-- | Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.  The number of transport stream packets per column error correction packet. Must be between 4 and 20, inclusive.
fosColumnDepth :: Lens' FecOutputSettings (Maybe Natural)
fosColumnDepth = lens _fosColumnDepth (\ s a -> s{_fosColumnDepth = a}) . mapping _Nat

instance FromJSON FecOutputSettings where
        parseJSON
          = withObject "FecOutputSettings"
              (\ x ->
                 FecOutputSettings' <$>
                   (x .:? "rowLength") <*> (x .:? "includeFec") <*>
                     (x .:? "columnDepth"))

instance Hashable FecOutputSettings where

instance NFData FecOutputSettings where

instance ToJSON FecOutputSettings where
        toJSON FecOutputSettings'{..}
          = object
              (catMaybes
                 [("rowLength" .=) <$> _fosRowLength,
                  ("includeFec" .=) <$> _fosIncludeFec,
                  ("columnDepth" .=) <$> _fosColumnDepth])

-- | Start time for the action.
--
-- /See:/ 'fixedModeScheduleActionStartSettings' smart constructor.
newtype FixedModeScheduleActionStartSettings = FixedModeScheduleActionStartSettings'{_fmsassTime
                                                                                     ::
                                                                                     Text}
                                                 deriving (Eq, Read, Show, Data,
                                                           Typeable, Generic)

-- | Creates a value of 'FixedModeScheduleActionStartSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmsassTime' - Start time for the action to start in the channel. (Not the time for the action to be added to the schedule: actions are always added to the schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the letters are digits (for example, mm might be 01) except for the two constants "T" for time and "Z" for "UTC format".
fixedModeScheduleActionStartSettings
    :: Text -- ^ 'fmsassTime'
    -> FixedModeScheduleActionStartSettings
fixedModeScheduleActionStartSettings pTime_
  = FixedModeScheduleActionStartSettings'{_fmsassTime =
                                            pTime_}

-- | Start time for the action to start in the channel. (Not the time for the action to be added to the schedule: actions are always added to the schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the letters are digits (for example, mm might be 01) except for the two constants "T" for time and "Z" for "UTC format".
fmsassTime :: Lens' FixedModeScheduleActionStartSettings Text
fmsassTime = lens _fmsassTime (\ s a -> s{_fmsassTime = a})

instance FromJSON
           FixedModeScheduleActionStartSettings
         where
        parseJSON
          = withObject "FixedModeScheduleActionStartSettings"
              (\ x ->
                 FixedModeScheduleActionStartSettings' <$>
                   (x .: "time"))

instance Hashable
           FixedModeScheduleActionStartSettings
         where

instance NFData FixedModeScheduleActionStartSettings
         where

instance ToJSON FixedModeScheduleActionStartSettings
         where
        toJSON FixedModeScheduleActionStartSettings'{..}
          = object (catMaybes [Just ("time" .= _fmsassTime)])

-- | Fmp4 Hls Settings
--
-- /See:/ 'fmp4HlsSettings' smart constructor.
data Fmp4HlsSettings = Fmp4HlsSettings'{_fhsNielsenId3Behavior
                                        :: !(Maybe Fmp4NielsenId3Behavior),
                                        _fhsAudioRenditionSets :: !(Maybe Text),
                                        _fhsTimedMetadataBehavior ::
                                        !(Maybe Fmp4TimedMetadataBehavior)}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Fmp4HlsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fhsNielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- * 'fhsAudioRenditionSets' - List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- * 'fhsTimedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to output.
fmp4HlsSettings
    :: Fmp4HlsSettings
fmp4HlsSettings
  = Fmp4HlsSettings'{_fhsNielsenId3Behavior = Nothing,
                     _fhsAudioRenditionSets = Nothing,
                     _fhsTimedMetadataBehavior = Nothing}

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
fhsNielsenId3Behavior :: Lens' Fmp4HlsSettings (Maybe Fmp4NielsenId3Behavior)
fhsNielsenId3Behavior = lens _fhsNielsenId3Behavior (\ s a -> s{_fhsNielsenId3Behavior = a})

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
fhsAudioRenditionSets :: Lens' Fmp4HlsSettings (Maybe Text)
fhsAudioRenditionSets = lens _fhsAudioRenditionSets (\ s a -> s{_fhsAudioRenditionSets = a})

-- | When set to passthrough, timed metadata is passed through from input to output.
fhsTimedMetadataBehavior :: Lens' Fmp4HlsSettings (Maybe Fmp4TimedMetadataBehavior)
fhsTimedMetadataBehavior = lens _fhsTimedMetadataBehavior (\ s a -> s{_fhsTimedMetadataBehavior = a})

instance FromJSON Fmp4HlsSettings where
        parseJSON
          = withObject "Fmp4HlsSettings"
              (\ x ->
                 Fmp4HlsSettings' <$>
                   (x .:? "nielsenId3Behavior") <*>
                     (x .:? "audioRenditionSets")
                     <*> (x .:? "timedMetadataBehavior"))

instance Hashable Fmp4HlsSettings where

instance NFData Fmp4HlsSettings where

instance ToJSON Fmp4HlsSettings where
        toJSON Fmp4HlsSettings'{..}
          = object
              (catMaybes
                 [("nielsenId3Behavior" .=) <$>
                    _fhsNielsenId3Behavior,
                  ("audioRenditionSets" .=) <$> _fhsAudioRenditionSets,
                  ("timedMetadataBehavior" .=) <$>
                    _fhsTimedMetadataBehavior])

-- | Settings to specify if an action follows another.
--
-- /See:/ 'followModeScheduleActionStartSettings' smart constructor.
data FollowModeScheduleActionStartSettings = FollowModeScheduleActionStartSettings'{_fmsassReferenceActionName
                                                                                    ::
                                                                                    !Text,
                                                                                    _fmsassFollowPoint
                                                                                    ::
                                                                                    !FollowPoint}
                                               deriving (Eq, Read, Show, Data,
                                                         Typeable, Generic)

-- | Creates a value of 'FollowModeScheduleActionStartSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmsassReferenceActionName' - The action name of another action that this one refers to.
--
-- * 'fmsassFollowPoint' - Identifies whether this action starts relative to the start or relative to the end of the reference action.
followModeScheduleActionStartSettings
    :: Text -- ^ 'fmsassReferenceActionName'
    -> FollowPoint -- ^ 'fmsassFollowPoint'
    -> FollowModeScheduleActionStartSettings
followModeScheduleActionStartSettings
  pReferenceActionName_ pFollowPoint_
  = FollowModeScheduleActionStartSettings'{_fmsassReferenceActionName
                                             = pReferenceActionName_,
                                           _fmsassFollowPoint = pFollowPoint_}

-- | The action name of another action that this one refers to.
fmsassReferenceActionName :: Lens' FollowModeScheduleActionStartSettings Text
fmsassReferenceActionName = lens _fmsassReferenceActionName (\ s a -> s{_fmsassReferenceActionName = a})

-- | Identifies whether this action starts relative to the start or relative to the end of the reference action.
fmsassFollowPoint :: Lens' FollowModeScheduleActionStartSettings FollowPoint
fmsassFollowPoint = lens _fmsassFollowPoint (\ s a -> s{_fmsassFollowPoint = a})

instance FromJSON
           FollowModeScheduleActionStartSettings
         where
        parseJSON
          = withObject "FollowModeScheduleActionStartSettings"
              (\ x ->
                 FollowModeScheduleActionStartSettings' <$>
                   (x .: "referenceActionName") <*>
                     (x .: "followPoint"))

instance Hashable
           FollowModeScheduleActionStartSettings
         where

instance NFData FollowModeScheduleActionStartSettings
         where

instance ToJSON FollowModeScheduleActionStartSettings
         where
        toJSON FollowModeScheduleActionStartSettings'{..}
          = object
              (catMaybes
                 [Just
                    ("referenceActionName" .=
                       _fmsassReferenceActionName),
                  Just ("followPoint" .= _fmsassFollowPoint)])

-- | Frame Capture Group Settings
--
-- /See:/ 'frameCaptureGroupSettings' smart constructor.
newtype FrameCaptureGroupSettings = FrameCaptureGroupSettings'{_fcgsDestination
                                                               ::
                                                               OutputLocationRef}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'FrameCaptureGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcgsDestination' - The destination for the frame capture files. Either the URI for an Amazon S3 bucket and object, plus a file name prefix (for example, s3ssl://sportsDelivery/highlights/20180820/curling_) or the URI for a MediaStore container, plus a file name prefix (for example, mediastoressl://sportsDelivery/20180820/curling_). The final file names consist of the prefix from the destination field (for example, "curling_") + name modifier + the counter (5 digits, starting from 00001) + extension (which is always .jpg).  For example, curlingLow.00001.jpg
frameCaptureGroupSettings
    :: OutputLocationRef -- ^ 'fcgsDestination'
    -> FrameCaptureGroupSettings
frameCaptureGroupSettings pDestination_
  = FrameCaptureGroupSettings'{_fcgsDestination =
                                 pDestination_}

-- | The destination for the frame capture files. Either the URI for an Amazon S3 bucket and object, plus a file name prefix (for example, s3ssl://sportsDelivery/highlights/20180820/curling_) or the URI for a MediaStore container, plus a file name prefix (for example, mediastoressl://sportsDelivery/20180820/curling_). The final file names consist of the prefix from the destination field (for example, "curling_") + name modifier + the counter (5 digits, starting from 00001) + extension (which is always .jpg).  For example, curlingLow.00001.jpg
fcgsDestination :: Lens' FrameCaptureGroupSettings OutputLocationRef
fcgsDestination = lens _fcgsDestination (\ s a -> s{_fcgsDestination = a})

instance FromJSON FrameCaptureGroupSettings where
        parseJSON
          = withObject "FrameCaptureGroupSettings"
              (\ x ->
                 FrameCaptureGroupSettings' <$> (x .: "destination"))

instance Hashable FrameCaptureGroupSettings where

instance NFData FrameCaptureGroupSettings where

instance ToJSON FrameCaptureGroupSettings where
        toJSON FrameCaptureGroupSettings'{..}
          = object
              (catMaybes
                 [Just ("destination" .= _fcgsDestination)])

-- | Frame Capture Output Settings
--
-- /See:/ 'frameCaptureOutputSettings' smart constructor.
newtype FrameCaptureOutputSettings = FrameCaptureOutputSettings'{_fcosNameModifier
                                                                 :: Maybe Text}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'FrameCaptureOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcosNameModifier' - Required if the output group contains more than one output. This modifier forms part of the output file name.
frameCaptureOutputSettings
    :: FrameCaptureOutputSettings
frameCaptureOutputSettings
  = FrameCaptureOutputSettings'{_fcosNameModifier =
                                  Nothing}

-- | Required if the output group contains more than one output. This modifier forms part of the output file name.
fcosNameModifier :: Lens' FrameCaptureOutputSettings (Maybe Text)
fcosNameModifier = lens _fcosNameModifier (\ s a -> s{_fcosNameModifier = a})

instance FromJSON FrameCaptureOutputSettings where
        parseJSON
          = withObject "FrameCaptureOutputSettings"
              (\ x ->
                 FrameCaptureOutputSettings' <$>
                   (x .:? "nameModifier"))

instance Hashable FrameCaptureOutputSettings where

instance NFData FrameCaptureOutputSettings where

instance ToJSON FrameCaptureOutputSettings where
        toJSON FrameCaptureOutputSettings'{..}
          = object
              (catMaybes
                 [("nameModifier" .=) <$> _fcosNameModifier])

-- | Frame Capture Settings
--
-- /See:/ 'frameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'{_fcsCaptureIntervalUnits
                                                  ::
                                                  !(Maybe
                                                      FrameCaptureIntervalUnit),
                                                  _fcsCaptureInterval :: !Nat}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FrameCaptureSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcsCaptureIntervalUnits' - Unit for the frame capture interval.
--
-- * 'fcsCaptureInterval' - The frequency at which to capture frames for inclusion in the output. May be specified in either seconds or milliseconds, as specified by captureIntervalUnits.
frameCaptureSettings
    :: Natural -- ^ 'fcsCaptureInterval'
    -> FrameCaptureSettings
frameCaptureSettings pCaptureInterval_
  = FrameCaptureSettings'{_fcsCaptureIntervalUnits =
                            Nothing,
                          _fcsCaptureInterval = _Nat # pCaptureInterval_}

-- | Unit for the frame capture interval.
fcsCaptureIntervalUnits :: Lens' FrameCaptureSettings (Maybe FrameCaptureIntervalUnit)
fcsCaptureIntervalUnits = lens _fcsCaptureIntervalUnits (\ s a -> s{_fcsCaptureIntervalUnits = a})

-- | The frequency at which to capture frames for inclusion in the output. May be specified in either seconds or milliseconds, as specified by captureIntervalUnits.
fcsCaptureInterval :: Lens' FrameCaptureSettings Natural
fcsCaptureInterval = lens _fcsCaptureInterval (\ s a -> s{_fcsCaptureInterval = a}) . _Nat

instance FromJSON FrameCaptureSettings where
        parseJSON
          = withObject "FrameCaptureSettings"
              (\ x ->
                 FrameCaptureSettings' <$>
                   (x .:? "captureIntervalUnits") <*>
                     (x .: "captureInterval"))

instance Hashable FrameCaptureSettings where

instance NFData FrameCaptureSettings where

instance ToJSON FrameCaptureSettings where
        toJSON FrameCaptureSettings'{..}
          = object
              (catMaybes
                 [("captureIntervalUnits" .=) <$>
                    _fcsCaptureIntervalUnits,
                  Just ("captureInterval" .= _fcsCaptureInterval)])

-- | Global Configuration
--
-- /See:/ 'globalConfiguration' smart constructor.
data GlobalConfiguration = GlobalConfiguration'{_gcOutputLockingMode
                                                ::
                                                !(Maybe
                                                    GlobalConfigurationOutputLockingMode),
                                                _gcInputLossBehavior ::
                                                !(Maybe InputLossBehavior),
                                                _gcInitialAudioGain ::
                                                !(Maybe Int),
                                                _gcSupportLowFramerateInputs ::
                                                !(Maybe
                                                    GlobalConfigurationLowFramerateInputs),
                                                _gcInputEndAction ::
                                                !(Maybe
                                                    GlobalConfigurationInputEndAction),
                                                _gcOutputTimingSource ::
                                                !(Maybe
                                                    GlobalConfigurationOutputTimingSource)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcOutputLockingMode' - Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the Unix epoch.
--
-- * 'gcInputLossBehavior' - Settings for system actions when input is lost.
--
-- * 'gcInitialAudioGain' - Value to set the initial audio gain for the Live Event.
--
-- * 'gcSupportLowFramerateInputs' - Adjusts video input buffer for streams with very low video framerates. This is commonly set to enabled for music channels with less than one video frame per second.
--
-- * 'gcInputEndAction' - Indicates the action to take when the current input completes (e.g. end-of-file). When switchAndLoopInputs is configured the encoder will restart at the beginning of the first input.  When "none" is configured the encoder will transcode either black, a solid color, or a user specified slate images per the "Input Loss Behavior" configuration until the next input switch occurs (which is controlled through the Channel Schedule API).
--
-- * 'gcOutputTimingSource' - Indicates whether the rate of frames emitted by the Live encoder should be paced by its system clock (which optionally may be locked to another source via NTP) or should be locked to the clock of the source that is providing the input stream.
globalConfiguration
    :: GlobalConfiguration
globalConfiguration
  = GlobalConfiguration'{_gcOutputLockingMode =
                           Nothing,
                         _gcInputLossBehavior = Nothing,
                         _gcInitialAudioGain = Nothing,
                         _gcSupportLowFramerateInputs = Nothing,
                         _gcInputEndAction = Nothing,
                         _gcOutputTimingSource = Nothing}

-- | Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the Unix epoch.
gcOutputLockingMode :: Lens' GlobalConfiguration (Maybe GlobalConfigurationOutputLockingMode)
gcOutputLockingMode = lens _gcOutputLockingMode (\ s a -> s{_gcOutputLockingMode = a})

-- | Settings for system actions when input is lost.
gcInputLossBehavior :: Lens' GlobalConfiguration (Maybe InputLossBehavior)
gcInputLossBehavior = lens _gcInputLossBehavior (\ s a -> s{_gcInputLossBehavior = a})

-- | Value to set the initial audio gain for the Live Event.
gcInitialAudioGain :: Lens' GlobalConfiguration (Maybe Int)
gcInitialAudioGain = lens _gcInitialAudioGain (\ s a -> s{_gcInitialAudioGain = a})

-- | Adjusts video input buffer for streams with very low video framerates. This is commonly set to enabled for music channels with less than one video frame per second.
gcSupportLowFramerateInputs :: Lens' GlobalConfiguration (Maybe GlobalConfigurationLowFramerateInputs)
gcSupportLowFramerateInputs = lens _gcSupportLowFramerateInputs (\ s a -> s{_gcSupportLowFramerateInputs = a})

-- | Indicates the action to take when the current input completes (e.g. end-of-file). When switchAndLoopInputs is configured the encoder will restart at the beginning of the first input.  When "none" is configured the encoder will transcode either black, a solid color, or a user specified slate images per the "Input Loss Behavior" configuration until the next input switch occurs (which is controlled through the Channel Schedule API).
gcInputEndAction :: Lens' GlobalConfiguration (Maybe GlobalConfigurationInputEndAction)
gcInputEndAction = lens _gcInputEndAction (\ s a -> s{_gcInputEndAction = a})

-- | Indicates whether the rate of frames emitted by the Live encoder should be paced by its system clock (which optionally may be locked to another source via NTP) or should be locked to the clock of the source that is providing the input stream.
gcOutputTimingSource :: Lens' GlobalConfiguration (Maybe GlobalConfigurationOutputTimingSource)
gcOutputTimingSource = lens _gcOutputTimingSource (\ s a -> s{_gcOutputTimingSource = a})

instance FromJSON GlobalConfiguration where
        parseJSON
          = withObject "GlobalConfiguration"
              (\ x ->
                 GlobalConfiguration' <$>
                   (x .:? "outputLockingMode") <*>
                     (x .:? "inputLossBehavior")
                     <*> (x .:? "initialAudioGain")
                     <*> (x .:? "supportLowFramerateInputs")
                     <*> (x .:? "inputEndAction")
                     <*> (x .:? "outputTimingSource"))

instance Hashable GlobalConfiguration where

instance NFData GlobalConfiguration where

instance ToJSON GlobalConfiguration where
        toJSON GlobalConfiguration'{..}
          = object
              (catMaybes
                 [("outputLockingMode" .=) <$> _gcOutputLockingMode,
                  ("inputLossBehavior" .=) <$> _gcInputLossBehavior,
                  ("initialAudioGain" .=) <$> _gcInitialAudioGain,
                  ("supportLowFramerateInputs" .=) <$>
                    _gcSupportLowFramerateInputs,
                  ("inputEndAction" .=) <$> _gcInputEndAction,
                  ("outputTimingSource" .=) <$> _gcOutputTimingSource])

-- | H264 Color Space Settings
--
-- /See:/ 'h264ColorSpaceSettings' smart constructor.
data H264ColorSpaceSettings = H264ColorSpaceSettings'{_hRec709Settings
                                                      ::
                                                      !(Maybe Rec709Settings),
                                                      _hRec601Settings ::
                                                      !(Maybe Rec601Settings),
                                                      _hColorSpacePassthroughSettings
                                                      ::
                                                      !(Maybe
                                                          ColorSpacePassthroughSettings)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'H264ColorSpaceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hRec709Settings' - Undocumented member.
--
-- * 'hRec601Settings' - Undocumented member.
--
-- * 'hColorSpacePassthroughSettings' - Undocumented member.
h264ColorSpaceSettings
    :: H264ColorSpaceSettings
h264ColorSpaceSettings
  = H264ColorSpaceSettings'{_hRec709Settings = Nothing,
                            _hRec601Settings = Nothing,
                            _hColorSpacePassthroughSettings = Nothing}

-- | Undocumented member.
hRec709Settings :: Lens' H264ColorSpaceSettings (Maybe Rec709Settings)
hRec709Settings = lens _hRec709Settings (\ s a -> s{_hRec709Settings = a})

-- | Undocumented member.
hRec601Settings :: Lens' H264ColorSpaceSettings (Maybe Rec601Settings)
hRec601Settings = lens _hRec601Settings (\ s a -> s{_hRec601Settings = a})

-- | Undocumented member.
hColorSpacePassthroughSettings :: Lens' H264ColorSpaceSettings (Maybe ColorSpacePassthroughSettings)
hColorSpacePassthroughSettings = lens _hColorSpacePassthroughSettings (\ s a -> s{_hColorSpacePassthroughSettings = a})

instance FromJSON H264ColorSpaceSettings where
        parseJSON
          = withObject "H264ColorSpaceSettings"
              (\ x ->
                 H264ColorSpaceSettings' <$>
                   (x .:? "rec709Settings") <*> (x .:? "rec601Settings")
                     <*> (x .:? "colorSpacePassthroughSettings"))

instance Hashable H264ColorSpaceSettings where

instance NFData H264ColorSpaceSettings where

instance ToJSON H264ColorSpaceSettings where
        toJSON H264ColorSpaceSettings'{..}
          = object
              (catMaybes
                 [("rec709Settings" .=) <$> _hRec709Settings,
                  ("rec601Settings" .=) <$> _hRec601Settings,
                  ("colorSpacePassthroughSettings" .=) <$>
                    _hColorSpacePassthroughSettings])

-- | H264 Filter Settings
--
-- /See:/ 'h264FilterSettings' smart constructor.
newtype H264FilterSettings = H264FilterSettings'{_hfsTemporalFilterSettings
                                                 ::
                                                 Maybe TemporalFilterSettings}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'H264FilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hfsTemporalFilterSettings' - Undocumented member.
h264FilterSettings
    :: H264FilterSettings
h264FilterSettings
  = H264FilterSettings'{_hfsTemporalFilterSettings =
                          Nothing}

-- | Undocumented member.
hfsTemporalFilterSettings :: Lens' H264FilterSettings (Maybe TemporalFilterSettings)
hfsTemporalFilterSettings = lens _hfsTemporalFilterSettings (\ s a -> s{_hfsTemporalFilterSettings = a})

instance FromJSON H264FilterSettings where
        parseJSON
          = withObject "H264FilterSettings"
              (\ x ->
                 H264FilterSettings' <$>
                   (x .:? "temporalFilterSettings"))

instance Hashable H264FilterSettings where

instance NFData H264FilterSettings where

instance ToJSON H264FilterSettings where
        toJSON H264FilterSettings'{..}
          = object
              (catMaybes
                 [("temporalFilterSettings" .=) <$>
                    _hfsTemporalFilterSettings])

-- | H264 Settings
--
-- /See:/ 'h264Settings' smart constructor.
data H264Settings = H264Settings'{_hssTemporalAq ::
                                  !(Maybe H264TemporalAq),
                                  _hssSceneChangeDetect ::
                                  !(Maybe H264SceneChangeDetect),
                                  _hssScanType :: !(Maybe H264ScanType),
                                  _hssTimecodeInsertion ::
                                  !(Maybe H264TimecodeInsertionBehavior),
                                  _hssParNumerator :: !(Maybe Int),
                                  _hssAfdSignaling :: !(Maybe AfdSignaling),
                                  _hssGopSize :: !(Maybe Double),
                                  _hssGopSizeUnits :: !(Maybe H264GopSizeUnits),
                                  _hssSubgopLength :: !(Maybe H264SubGopLength),
                                  _hssQualityLevel :: !(Maybe H264QualityLevel),
                                  _hssSlices :: !(Maybe Nat),
                                  _hssProfile :: !(Maybe H264Profile),
                                  _hssRateControlMode ::
                                  !(Maybe H264RateControlMode),
                                  _hssMinIInterval :: !(Maybe Nat),
                                  _hssQvbrQualityLevel :: !(Maybe Nat),
                                  _hssColorSpaceSettings ::
                                  !(Maybe H264ColorSpaceSettings),
                                  _hssParControl :: !(Maybe H264ParControl),
                                  _hssFlickerAq :: !(Maybe H264FlickerAq),
                                  _hssBufSize :: !(Maybe Nat),
                                  _hssSpatialAq :: !(Maybe H264SpatialAq),
                                  _hssGopNumBFrames :: !(Maybe Nat),
                                  _hssFixedAfd :: !(Maybe FixedAfd),
                                  _hssSoftness :: !(Maybe Nat),
                                  _hssFilterSettings ::
                                  !(Maybe H264FilterSettings),
                                  _hssBitrate :: !(Maybe Nat),
                                  _hssFramerateDenominator :: !(Maybe Nat),
                                  _hssForceFieldPictures ::
                                  !(Maybe H264ForceFieldPictures),
                                  _hssEntropyEncoding ::
                                  !(Maybe H264EntropyEncoding),
                                  _hssFramerateControl ::
                                  !(Maybe H264FramerateControl),
                                  _hssColorMetadata ::
                                  !(Maybe H264ColorMetadata),
                                  _hssLookAheadRateControl ::
                                  !(Maybe H264LookAheadRateControl),
                                  _hssAdaptiveQuantization ::
                                  !(Maybe H264AdaptiveQuantization),
                                  _hssFramerateNumerator :: !(Maybe Nat),
                                  _hssLevel :: !(Maybe H264Level),
                                  _hssGopBReference ::
                                  !(Maybe H264GopBReference),
                                  _hssMaxBitrate :: !(Maybe Nat),
                                  _hssSyntax :: !(Maybe H264Syntax),
                                  _hssBufFillPct :: !(Maybe Nat),
                                  _hssGopClosedCadence :: !(Maybe Nat),
                                  _hssNumRefFrames :: !(Maybe Nat),
                                  _hssParDenominator :: !(Maybe Nat)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H264Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hssTemporalAq' - If set to enabled, adjust quantization within each frame based on temporal variation of content complexity.
--
-- * 'hssSceneChangeDetect' - Scene change detection. - On: inserts I-frames when scene change is detected. - Off: does not force an I-frame when scene change is detected.
--
-- * 'hssScanType' - Sets the scan type of the output to progressive or top-field-first interlaced.
--
-- * 'hssTimecodeInsertion' - Determines how timecodes should be inserted into the video elementary stream. - 'disabled': Do not include timecodes - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
--
-- * 'hssParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'hssAfdSignaling' - Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
--
-- * 'hssGopSize' - GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
--
-- * 'hssGopSizeUnits' - Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
--
-- * 'hssSubgopLength' - If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to dynamic, optimize the number of B-frames used for each sub-GOP to improve visual quality.
--
-- * 'hssQualityLevel' - Leave as STANDARD_QUALITY or choose a different value (which might result in additional costs to run the channel). - ENHANCED_QUALITY: Produces a slightly better video quality without an increase in the bitrate. Has an effect only when the Rate control mode is QVBR or CBR. If this channel is in a MediaLive multiplex, the value must be ENHANCED_QUALITY. - STANDARD_QUALITY: Valid for any Rate control mode.
--
-- * 'hssSlices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures. This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
--
-- * 'hssProfile' - H.264 Profile.
--
-- * 'hssRateControlMode' - Rate control mode. QVBR: Quality will match the specified quality level except when it is constrained by the maximum bitrate.  Recommended if you or your viewers pay for bandwidth. VBR: Quality and bitrate vary, depending on the video complexity. Recommended instead of QVBR if you want to maintain a specific average bitrate over the duration of the channel. CBR: Quality varies, depending on the video complexity. Recommended only if you distribute your assets to devices that cannot handle variable bitrates. Multiplex: This rate control mode is only supported (and is required) when the video is being delivered to a MediaLive Multiplex in which case the rate control configuration is controlled by the properties within the Multiplex Program.
--
-- * 'hssMinIInterval' - Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hssQvbrQualityLevel' - Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
--
-- * 'hssColorSpaceSettings' - Color Space settings
--
-- * 'hssParControl' - This field indicates how the output pixel aspect ratio is specified.  If "specified" is selected then the output video pixel aspect ratio is determined by parNumerator and parDenominator, else if "initializeFromSource" is selected then the output pixsel aspect ratio will be set equal to the input video pixel aspect ratio of the first input.
--
-- * 'hssFlickerAq' - If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
--
-- * 'hssBufSize' - Size of buffer (HRD buffer model) in bits.
--
-- * 'hssSpatialAq' - If set to enabled, adjust quantization within each frame based on spatial variation of content complexity.
--
-- * 'hssGopNumBFrames' - Number of B-frames between reference frames.
--
-- * 'hssFixedAfd' - Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
--
-- * 'hssSoftness' - Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
--
-- * 'hssFilterSettings' - Optional filters that you can apply to an encode.
--
-- * 'hssBitrate' - Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
--
-- * 'hssFramerateDenominator' - Framerate denominator.
--
-- * 'hssForceFieldPictures' - This setting applies only when scan type is "interlaced." It controls whether coding is performed on a field basis or on a frame basis. (When the video is progressive, the coding is always performed on a frame basis.) enabled: Force MediaLive to code on a field basis, so that odd and even sets of fields are coded separately. disabled: Code the two sets of fields separately (on a field basis) or together (on a frame basis using PAFF), depending on what is most appropriate for the content.
--
-- * 'hssEntropyEncoding' - Entropy encoding mode.  Use cabac (must be in Main or High profile) or cavlc.
--
-- * 'hssFramerateControl' - This field indicates how the output video frame rate is specified.  If "specified" is selected then the output video frame rate is determined by framerateNumerator and framerateDenominator, else if "initializeFromSource" is selected then the output video frame rate will be set equal to the input video frame rate of the first input.
--
-- * 'hssColorMetadata' - Includes colorspace metadata in the output.
--
-- * 'hssLookAheadRateControl' - Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
--
-- * 'hssAdaptiveQuantization' - Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
--
-- * 'hssFramerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'hssLevel' - H.264 Level.
--
-- * 'hssGopBReference' - Documentation update needed
--
-- * 'hssMaxBitrate' - For QVBR: See the tooltip for Quality level For VBR: Set the maximum bitrate in order to accommodate expected spikes in the complexity of the video.
--
-- * 'hssSyntax' - Produces a bitstream compliant with SMPTE RP-2027.
--
-- * 'hssBufFillPct' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'hssGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'hssNumRefFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- * 'hssParDenominator' - Pixel Aspect Ratio denominator.
h264Settings
    :: H264Settings
h264Settings
  = H264Settings'{_hssTemporalAq = Nothing,
                  _hssSceneChangeDetect = Nothing,
                  _hssScanType = Nothing,
                  _hssTimecodeInsertion = Nothing,
                  _hssParNumerator = Nothing,
                  _hssAfdSignaling = Nothing, _hssGopSize = Nothing,
                  _hssGopSizeUnits = Nothing,
                  _hssSubgopLength = Nothing,
                  _hssQualityLevel = Nothing, _hssSlices = Nothing,
                  _hssProfile = Nothing, _hssRateControlMode = Nothing,
                  _hssMinIInterval = Nothing,
                  _hssQvbrQualityLevel = Nothing,
                  _hssColorSpaceSettings = Nothing,
                  _hssParControl = Nothing, _hssFlickerAq = Nothing,
                  _hssBufSize = Nothing, _hssSpatialAq = Nothing,
                  _hssGopNumBFrames = Nothing, _hssFixedAfd = Nothing,
                  _hssSoftness = Nothing, _hssFilterSettings = Nothing,
                  _hssBitrate = Nothing,
                  _hssFramerateDenominator = Nothing,
                  _hssForceFieldPictures = Nothing,
                  _hssEntropyEncoding = Nothing,
                  _hssFramerateControl = Nothing,
                  _hssColorMetadata = Nothing,
                  _hssLookAheadRateControl = Nothing,
                  _hssAdaptiveQuantization = Nothing,
                  _hssFramerateNumerator = Nothing,
                  _hssLevel = Nothing, _hssGopBReference = Nothing,
                  _hssMaxBitrate = Nothing, _hssSyntax = Nothing,
                  _hssBufFillPct = Nothing,
                  _hssGopClosedCadence = Nothing,
                  _hssNumRefFrames = Nothing,
                  _hssParDenominator = Nothing}

-- | If set to enabled, adjust quantization within each frame based on temporal variation of content complexity.
hssTemporalAq :: Lens' H264Settings (Maybe H264TemporalAq)
hssTemporalAq = lens _hssTemporalAq (\ s a -> s{_hssTemporalAq = a})

-- | Scene change detection. - On: inserts I-frames when scene change is detected. - Off: does not force an I-frame when scene change is detected.
hssSceneChangeDetect :: Lens' H264Settings (Maybe H264SceneChangeDetect)
hssSceneChangeDetect = lens _hssSceneChangeDetect (\ s a -> s{_hssSceneChangeDetect = a})

-- | Sets the scan type of the output to progressive or top-field-first interlaced.
hssScanType :: Lens' H264Settings (Maybe H264ScanType)
hssScanType = lens _hssScanType (\ s a -> s{_hssScanType = a})

-- | Determines how timecodes should be inserted into the video elementary stream. - 'disabled': Do not include timecodes - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
hssTimecodeInsertion :: Lens' H264Settings (Maybe H264TimecodeInsertionBehavior)
hssTimecodeInsertion = lens _hssTimecodeInsertion (\ s a -> s{_hssTimecodeInsertion = a})

-- | Pixel Aspect Ratio numerator.
hssParNumerator :: Lens' H264Settings (Maybe Int)
hssParNumerator = lens _hssParNumerator (\ s a -> s{_hssParNumerator = a})

-- | Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
hssAfdSignaling :: Lens' H264Settings (Maybe AfdSignaling)
hssAfdSignaling = lens _hssAfdSignaling (\ s a -> s{_hssAfdSignaling = a})

-- | GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
hssGopSize :: Lens' H264Settings (Maybe Double)
hssGopSize = lens _hssGopSize (\ s a -> s{_hssGopSize = a})

-- | Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
hssGopSizeUnits :: Lens' H264Settings (Maybe H264GopSizeUnits)
hssGopSizeUnits = lens _hssGopSizeUnits (\ s a -> s{_hssGopSizeUnits = a})

-- | If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to dynamic, optimize the number of B-frames used for each sub-GOP to improve visual quality.
hssSubgopLength :: Lens' H264Settings (Maybe H264SubGopLength)
hssSubgopLength = lens _hssSubgopLength (\ s a -> s{_hssSubgopLength = a})

-- | Leave as STANDARD_QUALITY or choose a different value (which might result in additional costs to run the channel). - ENHANCED_QUALITY: Produces a slightly better video quality without an increase in the bitrate. Has an effect only when the Rate control mode is QVBR or CBR. If this channel is in a MediaLive multiplex, the value must be ENHANCED_QUALITY. - STANDARD_QUALITY: Valid for any Rate control mode.
hssQualityLevel :: Lens' H264Settings (Maybe H264QualityLevel)
hssQualityLevel = lens _hssQualityLevel (\ s a -> s{_hssQualityLevel = a})

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures. This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
hssSlices :: Lens' H264Settings (Maybe Natural)
hssSlices = lens _hssSlices (\ s a -> s{_hssSlices = a}) . mapping _Nat

-- | H.264 Profile.
hssProfile :: Lens' H264Settings (Maybe H264Profile)
hssProfile = lens _hssProfile (\ s a -> s{_hssProfile = a})

-- | Rate control mode. QVBR: Quality will match the specified quality level except when it is constrained by the maximum bitrate.  Recommended if you or your viewers pay for bandwidth. VBR: Quality and bitrate vary, depending on the video complexity. Recommended instead of QVBR if you want to maintain a specific average bitrate over the duration of the channel. CBR: Quality varies, depending on the video complexity. Recommended only if you distribute your assets to devices that cannot handle variable bitrates. Multiplex: This rate control mode is only supported (and is required) when the video is being delivered to a MediaLive Multiplex in which case the rate control configuration is controlled by the properties within the Multiplex Program.
hssRateControlMode :: Lens' H264Settings (Maybe H264RateControlMode)
hssRateControlMode = lens _hssRateControlMode (\ s a -> s{_hssRateControlMode = a})

-- | Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hssMinIInterval :: Lens' H264Settings (Maybe Natural)
hssMinIInterval = lens _hssMinIInterval (\ s a -> s{_hssMinIInterval = a}) . mapping _Nat

-- | Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
hssQvbrQualityLevel :: Lens' H264Settings (Maybe Natural)
hssQvbrQualityLevel = lens _hssQvbrQualityLevel (\ s a -> s{_hssQvbrQualityLevel = a}) . mapping _Nat

-- | Color Space settings
hssColorSpaceSettings :: Lens' H264Settings (Maybe H264ColorSpaceSettings)
hssColorSpaceSettings = lens _hssColorSpaceSettings (\ s a -> s{_hssColorSpaceSettings = a})

-- | This field indicates how the output pixel aspect ratio is specified.  If "specified" is selected then the output video pixel aspect ratio is determined by parNumerator and parDenominator, else if "initializeFromSource" is selected then the output pixsel aspect ratio will be set equal to the input video pixel aspect ratio of the first input.
hssParControl :: Lens' H264Settings (Maybe H264ParControl)
hssParControl = lens _hssParControl (\ s a -> s{_hssParControl = a})

-- | If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
hssFlickerAq :: Lens' H264Settings (Maybe H264FlickerAq)
hssFlickerAq = lens _hssFlickerAq (\ s a -> s{_hssFlickerAq = a})

-- | Size of buffer (HRD buffer model) in bits.
hssBufSize :: Lens' H264Settings (Maybe Natural)
hssBufSize = lens _hssBufSize (\ s a -> s{_hssBufSize = a}) . mapping _Nat

-- | If set to enabled, adjust quantization within each frame based on spatial variation of content complexity.
hssSpatialAq :: Lens' H264Settings (Maybe H264SpatialAq)
hssSpatialAq = lens _hssSpatialAq (\ s a -> s{_hssSpatialAq = a})

-- | Number of B-frames between reference frames.
hssGopNumBFrames :: Lens' H264Settings (Maybe Natural)
hssGopNumBFrames = lens _hssGopNumBFrames (\ s a -> s{_hssGopNumBFrames = a}) . mapping _Nat

-- | Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
hssFixedAfd :: Lens' H264Settings (Maybe FixedAfd)
hssFixedAfd = lens _hssFixedAfd (\ s a -> s{_hssFixedAfd = a})

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
hssSoftness :: Lens' H264Settings (Maybe Natural)
hssSoftness = lens _hssSoftness (\ s a -> s{_hssSoftness = a}) . mapping _Nat

-- | Optional filters that you can apply to an encode.
hssFilterSettings :: Lens' H264Settings (Maybe H264FilterSettings)
hssFilterSettings = lens _hssFilterSettings (\ s a -> s{_hssFilterSettings = a})

-- | Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
hssBitrate :: Lens' H264Settings (Maybe Natural)
hssBitrate = lens _hssBitrate (\ s a -> s{_hssBitrate = a}) . mapping _Nat

-- | Framerate denominator.
hssFramerateDenominator :: Lens' H264Settings (Maybe Natural)
hssFramerateDenominator = lens _hssFramerateDenominator (\ s a -> s{_hssFramerateDenominator = a}) . mapping _Nat

-- | This setting applies only when scan type is "interlaced." It controls whether coding is performed on a field basis or on a frame basis. (When the video is progressive, the coding is always performed on a frame basis.) enabled: Force MediaLive to code on a field basis, so that odd and even sets of fields are coded separately. disabled: Code the two sets of fields separately (on a field basis) or together (on a frame basis using PAFF), depending on what is most appropriate for the content.
hssForceFieldPictures :: Lens' H264Settings (Maybe H264ForceFieldPictures)
hssForceFieldPictures = lens _hssForceFieldPictures (\ s a -> s{_hssForceFieldPictures = a})

-- | Entropy encoding mode.  Use cabac (must be in Main or High profile) or cavlc.
hssEntropyEncoding :: Lens' H264Settings (Maybe H264EntropyEncoding)
hssEntropyEncoding = lens _hssEntropyEncoding (\ s a -> s{_hssEntropyEncoding = a})

-- | This field indicates how the output video frame rate is specified.  If "specified" is selected then the output video frame rate is determined by framerateNumerator and framerateDenominator, else if "initializeFromSource" is selected then the output video frame rate will be set equal to the input video frame rate of the first input.
hssFramerateControl :: Lens' H264Settings (Maybe H264FramerateControl)
hssFramerateControl = lens _hssFramerateControl (\ s a -> s{_hssFramerateControl = a})

-- | Includes colorspace metadata in the output.
hssColorMetadata :: Lens' H264Settings (Maybe H264ColorMetadata)
hssColorMetadata = lens _hssColorMetadata (\ s a -> s{_hssColorMetadata = a})

-- | Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
hssLookAheadRateControl :: Lens' H264Settings (Maybe H264LookAheadRateControl)
hssLookAheadRateControl = lens _hssLookAheadRateControl (\ s a -> s{_hssLookAheadRateControl = a})

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
hssAdaptiveQuantization :: Lens' H264Settings (Maybe H264AdaptiveQuantization)
hssAdaptiveQuantization = lens _hssAdaptiveQuantization (\ s a -> s{_hssAdaptiveQuantization = a})

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
hssFramerateNumerator :: Lens' H264Settings (Maybe Natural)
hssFramerateNumerator = lens _hssFramerateNumerator (\ s a -> s{_hssFramerateNumerator = a}) . mapping _Nat

-- | H.264 Level.
hssLevel :: Lens' H264Settings (Maybe H264Level)
hssLevel = lens _hssLevel (\ s a -> s{_hssLevel = a})

-- | Documentation update needed
hssGopBReference :: Lens' H264Settings (Maybe H264GopBReference)
hssGopBReference = lens _hssGopBReference (\ s a -> s{_hssGopBReference = a})

-- | For QVBR: See the tooltip for Quality level For VBR: Set the maximum bitrate in order to accommodate expected spikes in the complexity of the video.
hssMaxBitrate :: Lens' H264Settings (Maybe Natural)
hssMaxBitrate = lens _hssMaxBitrate (\ s a -> s{_hssMaxBitrate = a}) . mapping _Nat

-- | Produces a bitstream compliant with SMPTE RP-2027.
hssSyntax :: Lens' H264Settings (Maybe H264Syntax)
hssSyntax = lens _hssSyntax (\ s a -> s{_hssSyntax = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
hssBufFillPct :: Lens' H264Settings (Maybe Natural)
hssBufFillPct = lens _hssBufFillPct (\ s a -> s{_hssBufFillPct = a}) . mapping _Nat

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hssGopClosedCadence :: Lens' H264Settings (Maybe Natural)
hssGopClosedCadence = lens _hssGopClosedCadence (\ s a -> s{_hssGopClosedCadence = a}) . mapping _Nat

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
hssNumRefFrames :: Lens' H264Settings (Maybe Natural)
hssNumRefFrames = lens _hssNumRefFrames (\ s a -> s{_hssNumRefFrames = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
hssParDenominator :: Lens' H264Settings (Maybe Natural)
hssParDenominator = lens _hssParDenominator (\ s a -> s{_hssParDenominator = a}) . mapping _Nat

instance FromJSON H264Settings where
        parseJSON
          = withObject "H264Settings"
              (\ x ->
                 H264Settings' <$>
                   (x .:? "temporalAq") <*> (x .:? "sceneChangeDetect")
                     <*> (x .:? "scanType")
                     <*> (x .:? "timecodeInsertion")
                     <*> (x .:? "parNumerator")
                     <*> (x .:? "afdSignaling")
                     <*> (x .:? "gopSize")
                     <*> (x .:? "gopSizeUnits")
                     <*> (x .:? "subgopLength")
                     <*> (x .:? "qualityLevel")
                     <*> (x .:? "slices")
                     <*> (x .:? "profile")
                     <*> (x .:? "rateControlMode")
                     <*> (x .:? "minIInterval")
                     <*> (x .:? "qvbrQualityLevel")
                     <*> (x .:? "colorSpaceSettings")
                     <*> (x .:? "parControl")
                     <*> (x .:? "flickerAq")
                     <*> (x .:? "bufSize")
                     <*> (x .:? "spatialAq")
                     <*> (x .:? "gopNumBFrames")
                     <*> (x .:? "fixedAfd")
                     <*> (x .:? "softness")
                     <*> (x .:? "filterSettings")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "framerateDenominator")
                     <*> (x .:? "forceFieldPictures")
                     <*> (x .:? "entropyEncoding")
                     <*> (x .:? "framerateControl")
                     <*> (x .:? "colorMetadata")
                     <*> (x .:? "lookAheadRateControl")
                     <*> (x .:? "adaptiveQuantization")
                     <*> (x .:? "framerateNumerator")
                     <*> (x .:? "level")
                     <*> (x .:? "gopBReference")
                     <*> (x .:? "maxBitrate")
                     <*> (x .:? "syntax")
                     <*> (x .:? "bufFillPct")
                     <*> (x .:? "gopClosedCadence")
                     <*> (x .:? "numRefFrames")
                     <*> (x .:? "parDenominator"))

instance Hashable H264Settings where

instance NFData H264Settings where

instance ToJSON H264Settings where
        toJSON H264Settings'{..}
          = object
              (catMaybes
                 [("temporalAq" .=) <$> _hssTemporalAq,
                  ("sceneChangeDetect" .=) <$> _hssSceneChangeDetect,
                  ("scanType" .=) <$> _hssScanType,
                  ("timecodeInsertion" .=) <$> _hssTimecodeInsertion,
                  ("parNumerator" .=) <$> _hssParNumerator,
                  ("afdSignaling" .=) <$> _hssAfdSignaling,
                  ("gopSize" .=) <$> _hssGopSize,
                  ("gopSizeUnits" .=) <$> _hssGopSizeUnits,
                  ("subgopLength" .=) <$> _hssSubgopLength,
                  ("qualityLevel" .=) <$> _hssQualityLevel,
                  ("slices" .=) <$> _hssSlices,
                  ("profile" .=) <$> _hssProfile,
                  ("rateControlMode" .=) <$> _hssRateControlMode,
                  ("minIInterval" .=) <$> _hssMinIInterval,
                  ("qvbrQualityLevel" .=) <$> _hssQvbrQualityLevel,
                  ("colorSpaceSettings" .=) <$> _hssColorSpaceSettings,
                  ("parControl" .=) <$> _hssParControl,
                  ("flickerAq" .=) <$> _hssFlickerAq,
                  ("bufSize" .=) <$> _hssBufSize,
                  ("spatialAq" .=) <$> _hssSpatialAq,
                  ("gopNumBFrames" .=) <$> _hssGopNumBFrames,
                  ("fixedAfd" .=) <$> _hssFixedAfd,
                  ("softness" .=) <$> _hssSoftness,
                  ("filterSettings" .=) <$> _hssFilterSettings,
                  ("bitrate" .=) <$> _hssBitrate,
                  ("framerateDenominator" .=) <$>
                    _hssFramerateDenominator,
                  ("forceFieldPictures" .=) <$> _hssForceFieldPictures,
                  ("entropyEncoding" .=) <$> _hssEntropyEncoding,
                  ("framerateControl" .=) <$> _hssFramerateControl,
                  ("colorMetadata" .=) <$> _hssColorMetadata,
                  ("lookAheadRateControl" .=) <$>
                    _hssLookAheadRateControl,
                  ("adaptiveQuantization" .=) <$>
                    _hssAdaptiveQuantization,
                  ("framerateNumerator" .=) <$> _hssFramerateNumerator,
                  ("level" .=) <$> _hssLevel,
                  ("gopBReference" .=) <$> _hssGopBReference,
                  ("maxBitrate" .=) <$> _hssMaxBitrate,
                  ("syntax" .=) <$> _hssSyntax,
                  ("bufFillPct" .=) <$> _hssBufFillPct,
                  ("gopClosedCadence" .=) <$> _hssGopClosedCadence,
                  ("numRefFrames" .=) <$> _hssNumRefFrames,
                  ("parDenominator" .=) <$> _hssParDenominator])

-- | H265 Color Space Settings
--
-- /See:/ 'h265ColorSpaceSettings' smart constructor.
data H265ColorSpaceSettings = H265ColorSpaceSettings'{_hcssHdr10Settings
                                                      :: !(Maybe Hdr10Settings),
                                                      _hcssRec709Settings ::
                                                      !(Maybe Rec709Settings),
                                                      _hcssRec601Settings ::
                                                      !(Maybe Rec601Settings),
                                                      _hcssColorSpacePassthroughSettings
                                                      ::
                                                      !(Maybe
                                                          ColorSpacePassthroughSettings)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'H265ColorSpaceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcssHdr10Settings' - Undocumented member.
--
-- * 'hcssRec709Settings' - Undocumented member.
--
-- * 'hcssRec601Settings' - Undocumented member.
--
-- * 'hcssColorSpacePassthroughSettings' - Undocumented member.
h265ColorSpaceSettings
    :: H265ColorSpaceSettings
h265ColorSpaceSettings
  = H265ColorSpaceSettings'{_hcssHdr10Settings =
                              Nothing,
                            _hcssRec709Settings = Nothing,
                            _hcssRec601Settings = Nothing,
                            _hcssColorSpacePassthroughSettings = Nothing}

-- | Undocumented member.
hcssHdr10Settings :: Lens' H265ColorSpaceSettings (Maybe Hdr10Settings)
hcssHdr10Settings = lens _hcssHdr10Settings (\ s a -> s{_hcssHdr10Settings = a})

-- | Undocumented member.
hcssRec709Settings :: Lens' H265ColorSpaceSettings (Maybe Rec709Settings)
hcssRec709Settings = lens _hcssRec709Settings (\ s a -> s{_hcssRec709Settings = a})

-- | Undocumented member.
hcssRec601Settings :: Lens' H265ColorSpaceSettings (Maybe Rec601Settings)
hcssRec601Settings = lens _hcssRec601Settings (\ s a -> s{_hcssRec601Settings = a})

-- | Undocumented member.
hcssColorSpacePassthroughSettings :: Lens' H265ColorSpaceSettings (Maybe ColorSpacePassthroughSettings)
hcssColorSpacePassthroughSettings = lens _hcssColorSpacePassthroughSettings (\ s a -> s{_hcssColorSpacePassthroughSettings = a})

instance FromJSON H265ColorSpaceSettings where
        parseJSON
          = withObject "H265ColorSpaceSettings"
              (\ x ->
                 H265ColorSpaceSettings' <$>
                   (x .:? "hdr10Settings") <*> (x .:? "rec709Settings")
                     <*> (x .:? "rec601Settings")
                     <*> (x .:? "colorSpacePassthroughSettings"))

instance Hashable H265ColorSpaceSettings where

instance NFData H265ColorSpaceSettings where

instance ToJSON H265ColorSpaceSettings where
        toJSON H265ColorSpaceSettings'{..}
          = object
              (catMaybes
                 [("hdr10Settings" .=) <$> _hcssHdr10Settings,
                  ("rec709Settings" .=) <$> _hcssRec709Settings,
                  ("rec601Settings" .=) <$> _hcssRec601Settings,
                  ("colorSpacePassthroughSettings" .=) <$>
                    _hcssColorSpacePassthroughSettings])

-- | H265 Settings
--
-- /See:/ 'h265Settings' smart constructor.
data H265Settings = H265Settings'{_hsSceneChangeDetect
                                  :: !(Maybe H265SceneChangeDetect),
                                  _hsScanType :: !(Maybe H265ScanType),
                                  _hsTimecodeInsertion ::
                                  !(Maybe H265TimecodeInsertionBehavior),
                                  _hsParNumerator :: !(Maybe Nat),
                                  _hsAfdSignaling :: !(Maybe AfdSignaling),
                                  _hsGopSize :: !(Maybe Double),
                                  _hsGopSizeUnits :: !(Maybe H265GopSizeUnits),
                                  _hsSlices :: !(Maybe Nat),
                                  _hsProfile :: !(Maybe H265Profile),
                                  _hsAlternativeTransferFunction ::
                                  !(Maybe H265AlternativeTransferFunction),
                                  _hsRateControlMode ::
                                  !(Maybe H265RateControlMode),
                                  _hsMinIInterval :: !(Maybe Nat),
                                  _hsQvbrQualityLevel :: !(Maybe Nat),
                                  _hsColorSpaceSettings ::
                                  !(Maybe H265ColorSpaceSettings),
                                  _hsFlickerAq :: !(Maybe H265FlickerAq),
                                  _hsBufSize :: !(Maybe Nat),
                                  _hsTier :: !(Maybe H265Tier),
                                  _hsFixedAfd :: !(Maybe FixedAfd),
                                  _hsBitrate :: !(Maybe Nat),
                                  _hsColorMetadata ::
                                  !(Maybe H265ColorMetadata),
                                  _hsLookAheadRateControl ::
                                  !(Maybe H265LookAheadRateControl),
                                  _hsAdaptiveQuantization ::
                                  !(Maybe H265AdaptiveQuantization),
                                  _hsLevel :: !(Maybe H265Level),
                                  _hsMaxBitrate :: !(Maybe Nat),
                                  _hsGopClosedCadence :: !(Maybe Nat),
                                  _hsParDenominator :: !(Maybe Nat),
                                  _hsFramerateNumerator :: !Nat,
                                  _hsFramerateDenominator :: !Nat}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H265Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsSceneChangeDetect' - Scene change detection.
--
-- * 'hsScanType' - Sets the scan type of the output to progressive or top-field-first interlaced.
--
-- * 'hsTimecodeInsertion' - Determines how timecodes should be inserted into the video elementary stream. - 'disabled': Do not include timecodes - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
--
-- * 'hsParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'hsAfdSignaling' - Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
--
-- * 'hsGopSize' - GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
--
-- * 'hsGopSizeUnits' - Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
--
-- * 'hsSlices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures. This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
--
-- * 'hsProfile' - H.265 Profile.
--
-- * 'hsAlternativeTransferFunction' - Whether or not EML should insert an Alternative Transfer Function SEI message to support backwards compatibility with non-HDR decoders and displays.
--
-- * 'hsRateControlMode' - Rate control mode. QVBR: Quality will match the specified quality level except when it is constrained by the maximum bitrate.  Recommended if you or your viewers pay for bandwidth. CBR: Quality varies, depending on the video complexity. Recommended only if you distribute your assets to devices that cannot handle variable bitrates.
--
-- * 'hsMinIInterval' - Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hsQvbrQualityLevel' - Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
--
-- * 'hsColorSpaceSettings' - Color Space settings
--
-- * 'hsFlickerAq' - If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
--
-- * 'hsBufSize' - Size of buffer (HRD buffer model) in bits.
--
-- * 'hsTier' - H.265 Tier.
--
-- * 'hsFixedAfd' - Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
--
-- * 'hsBitrate' - Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
--
-- * 'hsColorMetadata' - Includes colorspace metadata in the output.
--
-- * 'hsLookAheadRateControl' - Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
--
-- * 'hsAdaptiveQuantization' - Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
--
-- * 'hsLevel' - H.265 Level.
--
-- * 'hsMaxBitrate' - For QVBR: See the tooltip for Quality level
--
-- * 'hsGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'hsParDenominator' - Pixel Aspect Ratio denominator.
--
-- * 'hsFramerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'hsFramerateDenominator' - Framerate denominator.
h265Settings
    :: Natural -- ^ 'hsFramerateNumerator'
    -> Natural -- ^ 'hsFramerateDenominator'
    -> H265Settings
h265Settings pFramerateNumerator_
  pFramerateDenominator_
  = H265Settings'{_hsSceneChangeDetect = Nothing,
                  _hsScanType = Nothing,
                  _hsTimecodeInsertion = Nothing,
                  _hsParNumerator = Nothing, _hsAfdSignaling = Nothing,
                  _hsGopSize = Nothing, _hsGopSizeUnits = Nothing,
                  _hsSlices = Nothing, _hsProfile = Nothing,
                  _hsAlternativeTransferFunction = Nothing,
                  _hsRateControlMode = Nothing,
                  _hsMinIInterval = Nothing,
                  _hsQvbrQualityLevel = Nothing,
                  _hsColorSpaceSettings = Nothing,
                  _hsFlickerAq = Nothing, _hsBufSize = Nothing,
                  _hsTier = Nothing, _hsFixedAfd = Nothing,
                  _hsBitrate = Nothing, _hsColorMetadata = Nothing,
                  _hsLookAheadRateControl = Nothing,
                  _hsAdaptiveQuantization = Nothing,
                  _hsLevel = Nothing, _hsMaxBitrate = Nothing,
                  _hsGopClosedCadence = Nothing,
                  _hsParDenominator = Nothing,
                  _hsFramerateNumerator = _Nat # pFramerateNumerator_,
                  _hsFramerateDenominator =
                    _Nat # pFramerateDenominator_}

-- | Scene change detection.
hsSceneChangeDetect :: Lens' H265Settings (Maybe H265SceneChangeDetect)
hsSceneChangeDetect = lens _hsSceneChangeDetect (\ s a -> s{_hsSceneChangeDetect = a})

-- | Sets the scan type of the output to progressive or top-field-first interlaced.
hsScanType :: Lens' H265Settings (Maybe H265ScanType)
hsScanType = lens _hsScanType (\ s a -> s{_hsScanType = a})

-- | Determines how timecodes should be inserted into the video elementary stream. - 'disabled': Do not include timecodes - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
hsTimecodeInsertion :: Lens' H265Settings (Maybe H265TimecodeInsertionBehavior)
hsTimecodeInsertion = lens _hsTimecodeInsertion (\ s a -> s{_hsTimecodeInsertion = a})

-- | Pixel Aspect Ratio numerator.
hsParNumerator :: Lens' H265Settings (Maybe Natural)
hsParNumerator = lens _hsParNumerator (\ s a -> s{_hsParNumerator = a}) . mapping _Nat

-- | Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
hsAfdSignaling :: Lens' H265Settings (Maybe AfdSignaling)
hsAfdSignaling = lens _hsAfdSignaling (\ s a -> s{_hsAfdSignaling = a})

-- | GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
hsGopSize :: Lens' H265Settings (Maybe Double)
hsGopSize = lens _hsGopSize (\ s a -> s{_hsGopSize = a})

-- | Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
hsGopSizeUnits :: Lens' H265Settings (Maybe H265GopSizeUnits)
hsGopSizeUnits = lens _hsGopSizeUnits (\ s a -> s{_hsGopSizeUnits = a})

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures. This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
hsSlices :: Lens' H265Settings (Maybe Natural)
hsSlices = lens _hsSlices (\ s a -> s{_hsSlices = a}) . mapping _Nat

-- | H.265 Profile.
hsProfile :: Lens' H265Settings (Maybe H265Profile)
hsProfile = lens _hsProfile (\ s a -> s{_hsProfile = a})

-- | Whether or not EML should insert an Alternative Transfer Function SEI message to support backwards compatibility with non-HDR decoders and displays.
hsAlternativeTransferFunction :: Lens' H265Settings (Maybe H265AlternativeTransferFunction)
hsAlternativeTransferFunction = lens _hsAlternativeTransferFunction (\ s a -> s{_hsAlternativeTransferFunction = a})

-- | Rate control mode. QVBR: Quality will match the specified quality level except when it is constrained by the maximum bitrate.  Recommended if you or your viewers pay for bandwidth. CBR: Quality varies, depending on the video complexity. Recommended only if you distribute your assets to devices that cannot handle variable bitrates.
hsRateControlMode :: Lens' H265Settings (Maybe H265RateControlMode)
hsRateControlMode = lens _hsRateControlMode (\ s a -> s{_hsRateControlMode = a})

-- | Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hsMinIInterval :: Lens' H265Settings (Maybe Natural)
hsMinIInterval = lens _hsMinIInterval (\ s a -> s{_hsMinIInterval = a}) . mapping _Nat

-- | Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
hsQvbrQualityLevel :: Lens' H265Settings (Maybe Natural)
hsQvbrQualityLevel = lens _hsQvbrQualityLevel (\ s a -> s{_hsQvbrQualityLevel = a}) . mapping _Nat

-- | Color Space settings
hsColorSpaceSettings :: Lens' H265Settings (Maybe H265ColorSpaceSettings)
hsColorSpaceSettings = lens _hsColorSpaceSettings (\ s a -> s{_hsColorSpaceSettings = a})

-- | If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
hsFlickerAq :: Lens' H265Settings (Maybe H265FlickerAq)
hsFlickerAq = lens _hsFlickerAq (\ s a -> s{_hsFlickerAq = a})

-- | Size of buffer (HRD buffer model) in bits.
hsBufSize :: Lens' H265Settings (Maybe Natural)
hsBufSize = lens _hsBufSize (\ s a -> s{_hsBufSize = a}) . mapping _Nat

-- | H.265 Tier.
hsTier :: Lens' H265Settings (Maybe H265Tier)
hsTier = lens _hsTier (\ s a -> s{_hsTier = a})

-- | Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
hsFixedAfd :: Lens' H265Settings (Maybe FixedAfd)
hsFixedAfd = lens _hsFixedAfd (\ s a -> s{_hsFixedAfd = a})

-- | Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
hsBitrate :: Lens' H265Settings (Maybe Natural)
hsBitrate = lens _hsBitrate (\ s a -> s{_hsBitrate = a}) . mapping _Nat

-- | Includes colorspace metadata in the output.
hsColorMetadata :: Lens' H265Settings (Maybe H265ColorMetadata)
hsColorMetadata = lens _hsColorMetadata (\ s a -> s{_hsColorMetadata = a})

-- | Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
hsLookAheadRateControl :: Lens' H265Settings (Maybe H265LookAheadRateControl)
hsLookAheadRateControl = lens _hsLookAheadRateControl (\ s a -> s{_hsLookAheadRateControl = a})

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
hsAdaptiveQuantization :: Lens' H265Settings (Maybe H265AdaptiveQuantization)
hsAdaptiveQuantization = lens _hsAdaptiveQuantization (\ s a -> s{_hsAdaptiveQuantization = a})

-- | H.265 Level.
hsLevel :: Lens' H265Settings (Maybe H265Level)
hsLevel = lens _hsLevel (\ s a -> s{_hsLevel = a})

-- | For QVBR: See the tooltip for Quality level
hsMaxBitrate :: Lens' H265Settings (Maybe Natural)
hsMaxBitrate = lens _hsMaxBitrate (\ s a -> s{_hsMaxBitrate = a}) . mapping _Nat

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hsGopClosedCadence :: Lens' H265Settings (Maybe Natural)
hsGopClosedCadence = lens _hsGopClosedCadence (\ s a -> s{_hsGopClosedCadence = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
hsParDenominator :: Lens' H265Settings (Maybe Natural)
hsParDenominator = lens _hsParDenominator (\ s a -> s{_hsParDenominator = a}) . mapping _Nat

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
hsFramerateNumerator :: Lens' H265Settings Natural
hsFramerateNumerator = lens _hsFramerateNumerator (\ s a -> s{_hsFramerateNumerator = a}) . _Nat

-- | Framerate denominator.
hsFramerateDenominator :: Lens' H265Settings Natural
hsFramerateDenominator = lens _hsFramerateDenominator (\ s a -> s{_hsFramerateDenominator = a}) . _Nat

instance FromJSON H265Settings where
        parseJSON
          = withObject "H265Settings"
              (\ x ->
                 H265Settings' <$>
                   (x .:? "sceneChangeDetect") <*> (x .:? "scanType")
                     <*> (x .:? "timecodeInsertion")
                     <*> (x .:? "parNumerator")
                     <*> (x .:? "afdSignaling")
                     <*> (x .:? "gopSize")
                     <*> (x .:? "gopSizeUnits")
                     <*> (x .:? "slices")
                     <*> (x .:? "profile")
                     <*> (x .:? "alternativeTransferFunction")
                     <*> (x .:? "rateControlMode")
                     <*> (x .:? "minIInterval")
                     <*> (x .:? "qvbrQualityLevel")
                     <*> (x .:? "colorSpaceSettings")
                     <*> (x .:? "flickerAq")
                     <*> (x .:? "bufSize")
                     <*> (x .:? "tier")
                     <*> (x .:? "fixedAfd")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "colorMetadata")
                     <*> (x .:? "lookAheadRateControl")
                     <*> (x .:? "adaptiveQuantization")
                     <*> (x .:? "level")
                     <*> (x .:? "maxBitrate")
                     <*> (x .:? "gopClosedCadence")
                     <*> (x .:? "parDenominator")
                     <*> (x .: "framerateNumerator")
                     <*> (x .: "framerateDenominator"))

instance Hashable H265Settings where

instance NFData H265Settings where

instance ToJSON H265Settings where
        toJSON H265Settings'{..}
          = object
              (catMaybes
                 [("sceneChangeDetect" .=) <$> _hsSceneChangeDetect,
                  ("scanType" .=) <$> _hsScanType,
                  ("timecodeInsertion" .=) <$> _hsTimecodeInsertion,
                  ("parNumerator" .=) <$> _hsParNumerator,
                  ("afdSignaling" .=) <$> _hsAfdSignaling,
                  ("gopSize" .=) <$> _hsGopSize,
                  ("gopSizeUnits" .=) <$> _hsGopSizeUnits,
                  ("slices" .=) <$> _hsSlices,
                  ("profile" .=) <$> _hsProfile,
                  ("alternativeTransferFunction" .=) <$>
                    _hsAlternativeTransferFunction,
                  ("rateControlMode" .=) <$> _hsRateControlMode,
                  ("minIInterval" .=) <$> _hsMinIInterval,
                  ("qvbrQualityLevel" .=) <$> _hsQvbrQualityLevel,
                  ("colorSpaceSettings" .=) <$> _hsColorSpaceSettings,
                  ("flickerAq" .=) <$> _hsFlickerAq,
                  ("bufSize" .=) <$> _hsBufSize,
                  ("tier" .=) <$> _hsTier,
                  ("fixedAfd" .=) <$> _hsFixedAfd,
                  ("bitrate" .=) <$> _hsBitrate,
                  ("colorMetadata" .=) <$> _hsColorMetadata,
                  ("lookAheadRateControl" .=) <$>
                    _hsLookAheadRateControl,
                  ("adaptiveQuantization" .=) <$>
                    _hsAdaptiveQuantization,
                  ("level" .=) <$> _hsLevel,
                  ("maxBitrate" .=) <$> _hsMaxBitrate,
                  ("gopClosedCadence" .=) <$> _hsGopClosedCadence,
                  ("parDenominator" .=) <$> _hsParDenominator,
                  Just ("framerateNumerator" .= _hsFramerateNumerator),
                  Just
                    ("framerateDenominator" .= _hsFramerateDenominator)])

-- | Hdr10 Settings
--
-- /See:/ 'hdr10Settings' smart constructor.
data Hdr10Settings = Hdr10Settings'{_hsMaxFall ::
                                    !(Maybe Nat),
                                    _hsMaxCll :: !(Maybe Nat)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Hdr10Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsMaxFall' - Maximum Frame Average Light Level An integer metadata value defining the maximum average light level, in nits, for any single frame within an encoded HDR video stream or file.
--
-- * 'hsMaxCll' - Maximum Content Light Level An integer metadata value defining the maximum light level, in nits, of any single pixel within an encoded HDR video stream or file.
hdr10Settings
    :: Hdr10Settings
hdr10Settings
  = Hdr10Settings'{_hsMaxFall = Nothing,
                   _hsMaxCll = Nothing}

-- | Maximum Frame Average Light Level An integer metadata value defining the maximum average light level, in nits, for any single frame within an encoded HDR video stream or file.
hsMaxFall :: Lens' Hdr10Settings (Maybe Natural)
hsMaxFall = lens _hsMaxFall (\ s a -> s{_hsMaxFall = a}) . mapping _Nat

-- | Maximum Content Light Level An integer metadata value defining the maximum light level, in nits, of any single pixel within an encoded HDR video stream or file.
hsMaxCll :: Lens' Hdr10Settings (Maybe Natural)
hsMaxCll = lens _hsMaxCll (\ s a -> s{_hsMaxCll = a}) . mapping _Nat

instance FromJSON Hdr10Settings where
        parseJSON
          = withObject "Hdr10Settings"
              (\ x ->
                 Hdr10Settings' <$>
                   (x .:? "maxFall") <*> (x .:? "maxCll"))

instance Hashable Hdr10Settings where

instance NFData Hdr10Settings where

instance ToJSON Hdr10Settings where
        toJSON Hdr10Settings'{..}
          = object
              (catMaybes
                 [("maxFall" .=) <$> _hsMaxFall,
                  ("maxCll" .=) <$> _hsMaxCll])

-- | Hls Akamai Settings
--
-- /See:/ 'hlsAkamaiSettings' smart constructor.
data HlsAkamaiSettings = HlsAkamaiSettings'{_hasHTTPTransferMode
                                            ::
                                            !(Maybe HlsAkamaiHTTPTransferMode),
                                            _hasNumRetries :: !(Maybe Nat),
                                            _hasToken :: !(Maybe Text),
                                            _hasConnectionRetryInterval ::
                                            !(Maybe Nat),
                                            _hasFilecacheDuration ::
                                            !(Maybe Nat),
                                            _hasRestartDelay :: !(Maybe Nat),
                                            _hasSalt :: !(Maybe Text)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsAkamaiSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hasHTTPTransferMode' - Specify whether or not to use chunked transfer encoding to Akamai. User should contact Akamai to enable this feature.
--
-- * 'hasNumRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- * 'hasToken' - Token parameter for authenticated akamai. If not specified, _gda_ is used.
--
-- * 'hasConnectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- * 'hasFilecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- * 'hasRestartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- * 'hasSalt' - Salt for authenticated Akamai.
hlsAkamaiSettings
    :: HlsAkamaiSettings
hlsAkamaiSettings
  = HlsAkamaiSettings'{_hasHTTPTransferMode = Nothing,
                       _hasNumRetries = Nothing, _hasToken = Nothing,
                       _hasConnectionRetryInterval = Nothing,
                       _hasFilecacheDuration = Nothing,
                       _hasRestartDelay = Nothing, _hasSalt = Nothing}

-- | Specify whether or not to use chunked transfer encoding to Akamai. User should contact Akamai to enable this feature.
hasHTTPTransferMode :: Lens' HlsAkamaiSettings (Maybe HlsAkamaiHTTPTransferMode)
hasHTTPTransferMode = lens _hasHTTPTransferMode (\ s a -> s{_hasHTTPTransferMode = a})

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
hasNumRetries :: Lens' HlsAkamaiSettings (Maybe Natural)
hasNumRetries = lens _hasNumRetries (\ s a -> s{_hasNumRetries = a}) . mapping _Nat

-- | Token parameter for authenticated akamai. If not specified, _gda_ is used.
hasToken :: Lens' HlsAkamaiSettings (Maybe Text)
hasToken = lens _hasToken (\ s a -> s{_hasToken = a})

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
hasConnectionRetryInterval :: Lens' HlsAkamaiSettings (Maybe Natural)
hasConnectionRetryInterval = lens _hasConnectionRetryInterval (\ s a -> s{_hasConnectionRetryInterval = a}) . mapping _Nat

-- | Size in seconds of file cache for streaming outputs.
hasFilecacheDuration :: Lens' HlsAkamaiSettings (Maybe Natural)
hasFilecacheDuration = lens _hasFilecacheDuration (\ s a -> s{_hasFilecacheDuration = a}) . mapping _Nat

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hasRestartDelay :: Lens' HlsAkamaiSettings (Maybe Natural)
hasRestartDelay = lens _hasRestartDelay (\ s a -> s{_hasRestartDelay = a}) . mapping _Nat

-- | Salt for authenticated Akamai.
hasSalt :: Lens' HlsAkamaiSettings (Maybe Text)
hasSalt = lens _hasSalt (\ s a -> s{_hasSalt = a})

instance FromJSON HlsAkamaiSettings where
        parseJSON
          = withObject "HlsAkamaiSettings"
              (\ x ->
                 HlsAkamaiSettings' <$>
                   (x .:? "httpTransferMode") <*> (x .:? "numRetries")
                     <*> (x .:? "token")
                     <*> (x .:? "connectionRetryInterval")
                     <*> (x .:? "filecacheDuration")
                     <*> (x .:? "restartDelay")
                     <*> (x .:? "salt"))

instance Hashable HlsAkamaiSettings where

instance NFData HlsAkamaiSettings where

instance ToJSON HlsAkamaiSettings where
        toJSON HlsAkamaiSettings'{..}
          = object
              (catMaybes
                 [("httpTransferMode" .=) <$> _hasHTTPTransferMode,
                  ("numRetries" .=) <$> _hasNumRetries,
                  ("token" .=) <$> _hasToken,
                  ("connectionRetryInterval" .=) <$>
                    _hasConnectionRetryInterval,
                  ("filecacheDuration" .=) <$> _hasFilecacheDuration,
                  ("restartDelay" .=) <$> _hasRestartDelay,
                  ("salt" .=) <$> _hasSalt])

-- | Hls Basic Put Settings
--
-- /See:/ 'hlsBasicPutSettings' smart constructor.
data HlsBasicPutSettings = HlsBasicPutSettings'{_hbpsNumRetries
                                                :: !(Maybe Nat),
                                                _hbpsConnectionRetryInterval ::
                                                !(Maybe Nat),
                                                _hbpsFilecacheDuration ::
                                                !(Maybe Nat),
                                                _hbpsRestartDelay ::
                                                !(Maybe Nat)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsBasicPutSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hbpsNumRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- * 'hbpsConnectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- * 'hbpsFilecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- * 'hbpsRestartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hlsBasicPutSettings
    :: HlsBasicPutSettings
hlsBasicPutSettings
  = HlsBasicPutSettings'{_hbpsNumRetries = Nothing,
                         _hbpsConnectionRetryInterval = Nothing,
                         _hbpsFilecacheDuration = Nothing,
                         _hbpsRestartDelay = Nothing}

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
hbpsNumRetries :: Lens' HlsBasicPutSettings (Maybe Natural)
hbpsNumRetries = lens _hbpsNumRetries (\ s a -> s{_hbpsNumRetries = a}) . mapping _Nat

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
hbpsConnectionRetryInterval :: Lens' HlsBasicPutSettings (Maybe Natural)
hbpsConnectionRetryInterval = lens _hbpsConnectionRetryInterval (\ s a -> s{_hbpsConnectionRetryInterval = a}) . mapping _Nat

-- | Size in seconds of file cache for streaming outputs.
hbpsFilecacheDuration :: Lens' HlsBasicPutSettings (Maybe Natural)
hbpsFilecacheDuration = lens _hbpsFilecacheDuration (\ s a -> s{_hbpsFilecacheDuration = a}) . mapping _Nat

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hbpsRestartDelay :: Lens' HlsBasicPutSettings (Maybe Natural)
hbpsRestartDelay = lens _hbpsRestartDelay (\ s a -> s{_hbpsRestartDelay = a}) . mapping _Nat

instance FromJSON HlsBasicPutSettings where
        parseJSON
          = withObject "HlsBasicPutSettings"
              (\ x ->
                 HlsBasicPutSettings' <$>
                   (x .:? "numRetries") <*>
                     (x .:? "connectionRetryInterval")
                     <*> (x .:? "filecacheDuration")
                     <*> (x .:? "restartDelay"))

instance Hashable HlsBasicPutSettings where

instance NFData HlsBasicPutSettings where

instance ToJSON HlsBasicPutSettings where
        toJSON HlsBasicPutSettings'{..}
          = object
              (catMaybes
                 [("numRetries" .=) <$> _hbpsNumRetries,
                  ("connectionRetryInterval" .=) <$>
                    _hbpsConnectionRetryInterval,
                  ("filecacheDuration" .=) <$> _hbpsFilecacheDuration,
                  ("restartDelay" .=) <$> _hbpsRestartDelay])

-- | Hls Cdn Settings
--
-- /See:/ 'hlsCdnSettings' smart constructor.
data HlsCdnSettings = HlsCdnSettings'{_hcsHlsAkamaiSettings
                                      :: !(Maybe HlsAkamaiSettings),
                                      _hcsHlsMediaStoreSettings ::
                                      !(Maybe HlsMediaStoreSettings),
                                      _hcsHlsBasicPutSettings ::
                                      !(Maybe HlsBasicPutSettings),
                                      _hcsHlsWebdavSettings ::
                                      !(Maybe HlsWebdavSettings)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsCdnSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcsHlsAkamaiSettings' - Undocumented member.
--
-- * 'hcsHlsMediaStoreSettings' - Undocumented member.
--
-- * 'hcsHlsBasicPutSettings' - Undocumented member.
--
-- * 'hcsHlsWebdavSettings' - Undocumented member.
hlsCdnSettings
    :: HlsCdnSettings
hlsCdnSettings
  = HlsCdnSettings'{_hcsHlsAkamaiSettings = Nothing,
                    _hcsHlsMediaStoreSettings = Nothing,
                    _hcsHlsBasicPutSettings = Nothing,
                    _hcsHlsWebdavSettings = Nothing}

-- | Undocumented member.
hcsHlsAkamaiSettings :: Lens' HlsCdnSettings (Maybe HlsAkamaiSettings)
hcsHlsAkamaiSettings = lens _hcsHlsAkamaiSettings (\ s a -> s{_hcsHlsAkamaiSettings = a})

-- | Undocumented member.
hcsHlsMediaStoreSettings :: Lens' HlsCdnSettings (Maybe HlsMediaStoreSettings)
hcsHlsMediaStoreSettings = lens _hcsHlsMediaStoreSettings (\ s a -> s{_hcsHlsMediaStoreSettings = a})

-- | Undocumented member.
hcsHlsBasicPutSettings :: Lens' HlsCdnSettings (Maybe HlsBasicPutSettings)
hcsHlsBasicPutSettings = lens _hcsHlsBasicPutSettings (\ s a -> s{_hcsHlsBasicPutSettings = a})

-- | Undocumented member.
hcsHlsWebdavSettings :: Lens' HlsCdnSettings (Maybe HlsWebdavSettings)
hcsHlsWebdavSettings = lens _hcsHlsWebdavSettings (\ s a -> s{_hcsHlsWebdavSettings = a})

instance FromJSON HlsCdnSettings where
        parseJSON
          = withObject "HlsCdnSettings"
              (\ x ->
                 HlsCdnSettings' <$>
                   (x .:? "hlsAkamaiSettings") <*>
                     (x .:? "hlsMediaStoreSettings")
                     <*> (x .:? "hlsBasicPutSettings")
                     <*> (x .:? "hlsWebdavSettings"))

instance Hashable HlsCdnSettings where

instance NFData HlsCdnSettings where

instance ToJSON HlsCdnSettings where
        toJSON HlsCdnSettings'{..}
          = object
              (catMaybes
                 [("hlsAkamaiSettings" .=) <$> _hcsHlsAkamaiSettings,
                  ("hlsMediaStoreSettings" .=) <$>
                    _hcsHlsMediaStoreSettings,
                  ("hlsBasicPutSettings" .=) <$>
                    _hcsHlsBasicPutSettings,
                  ("hlsWebdavSettings" .=) <$> _hcsHlsWebdavSettings])

-- | Hls Group Settings
--
-- /See:/ 'hlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'{_hgsDirectoryStructure
                                          :: !(Maybe HlsDirectoryStructure),
                                          _hgsEncryptionType ::
                                          !(Maybe HlsEncryptionType),
                                          _hgsTimedMetadataId3Period ::
                                          !(Maybe Nat),
                                          _hgsIvInManifest ::
                                          !(Maybe HlsIvInManifest),
                                          _hgsTsFileMode ::
                                          !(Maybe HlsTsFileMode),
                                          _hgsMinSegmentLength :: !(Maybe Nat),
                                          _hgsIFrameOnlyPlaylists ::
                                          !(Maybe IFrameOnlyPlaylistType),
                                          _hgsProgramDateTime ::
                                          !(Maybe HlsProgramDateTime),
                                          _hgsIndexNSegments :: !(Maybe Nat),
                                          _hgsProgramDateTimePeriod ::
                                          !(Maybe Nat),
                                          _hgsCodecSpecification ::
                                          !(Maybe HlsCodecSpecification),
                                          _hgsHlsCdnSettings ::
                                          !(Maybe HlsCdnSettings),
                                          _hgsCaptionLanguageMappings ::
                                          !(Maybe [CaptionLanguageMapping]),
                                          _hgsInputLossAction ::
                                          !(Maybe InputLossActionForHlsOut),
                                          _hgsMode :: !(Maybe HlsMode),
                                          _hgsKeyProviderSettings ::
                                          !(Maybe KeyProviderSettings),
                                          _hgsConstantIv :: !(Maybe Text),
                                          _hgsBaseURLManifest :: !(Maybe Text),
                                          _hgsAdMarkers ::
                                          !(Maybe [HlsAdMarkers]),
                                          _hgsKeyFormat :: !(Maybe Text),
                                          _hgsSegmentLength :: !(Maybe Nat),
                                          _hgsHlsId3SegmentTagging ::
                                          !(Maybe HlsId3SegmentTaggingState),
                                          _hgsTimedMetadataId3Frame ::
                                          !(Maybe HlsTimedMetadataId3Frame),
                                          _hgsBaseURLContent :: !(Maybe Text),
                                          _hgsOutputSelection ::
                                          !(Maybe HlsOutputSelection),
                                          _hgsCaptionLanguageSetting ::
                                          !(Maybe HlsCaptionLanguageSetting),
                                          _hgsSegmentsPerSubdirectory ::
                                          !(Maybe Nat),
                                          _hgsManifestDurationFormat ::
                                          !(Maybe HlsManifestDurationFormat),
                                          _hgsIvSource :: !(Maybe HlsIvSource),
                                          _hgsSegmentationMode ::
                                          !(Maybe HlsSegmentationMode),
                                          _hgsKeyFormatVersions ::
                                          !(Maybe Text),
                                          _hgsClientCache ::
                                          !(Maybe HlsClientCache),
                                          _hgsTimestampDeltaMilliseconds ::
                                          !(Maybe Nat),
                                          _hgsBaseURLManifest1 :: !(Maybe Text),
                                          _hgsRedundantManifest ::
                                          !(Maybe HlsRedundantManifest),
                                          _hgsStreamInfResolution ::
                                          !(Maybe HlsStreamInfResolution),
                                          _hgsKeepSegments :: !(Maybe Nat),
                                          _hgsBaseURLContent1 :: !(Maybe Text),
                                          _hgsManifestCompression ::
                                          !(Maybe HlsManifestCompression),
                                          _hgsDestination :: !OutputLocationRef}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hgsDirectoryStructure' - Place segments in subdirectories.
--
-- * 'hgsEncryptionType' - Encrypts the segments with the given encryption scheme.  Exclude this parameter if no encryption is desired.
--
-- * 'hgsTimedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- * 'hgsIvInManifest' - For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If set to "include", IV is listed in the manifest, otherwise the IV is not in the manifest.
--
-- * 'hgsTsFileMode' - SEGMENTED_FILES: Emit the program as segments - multiple .ts media files. SINGLE_FILE: Applies only if Mode field is VOD. Emit the program as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE tags to index segments for playback. A typical use for this value is when sending the output to AWS Elemental MediaConvert, which can accept only a single media file. Playback while the channel is running is not guaranteed due to HTTP server caching.
--
-- * 'hgsMinSegmentLength' - When set, minimumSegmentLength is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
--
-- * 'hgsIFrameOnlyPlaylists' - DISABLED: Do not create an I-frame-only manifest, but do create the master and media manifests (according to the Output Selection field). STANDARD: Create an I-frame-only manifest for each output that contains video, as well as the other manifests (according to the Output Selection field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries identifying the I-frame position. For example, #EXT-X-BYTERANGE:160364@1461888"
--
-- * 'hgsProgramDateTime' - Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestampOffset.
--
-- * 'hgsIndexNSegments' - Applies only if Mode field is LIVE. Specifies the maximum number of segments in the media manifest file. After this maximum, older segments are removed from the media manifest. This number must be less than or equal to the Keep Segments field.
--
-- * 'hgsProgramDateTimePeriod' - Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
--
-- * 'hgsCodecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
--
-- * 'hgsHlsCdnSettings' - Parameters that control interactions with the CDN.
--
-- * 'hgsCaptionLanguageMappings' - Mapping of up to 4 caption channels to caption languages.  Is only meaningful if captionLanguageSetting is set to "insert".
--
-- * 'hgsInputLossAction' - Parameter that control output group behavior on input loss.
--
-- * 'hgsMode' - If "vod", all segments are indexed and kept permanently in the destination and manifest. If "live", only the number segments specified in keepSegments and indexNSegments are kept; newer segments replace older segments, which may prevent players from rewinding all the way to the beginning of the event. VOD mode uses HLS EXT-X-PLAYLIST-TYPE of EVENT while the channel is running, converting it to a "VOD" type manifest on completion of the stream.
--
-- * 'hgsKeyProviderSettings' - The key provider settings.
--
-- * 'hgsConstantIv' - For use with encryptionType. This is a 128-bit, 16-byte hex value represented by a 32-character text string. If ivSource is set to "explicit" then this parameter is required and is used as the IV for encryption.
--
-- * 'hgsBaseURLManifest' - A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- * 'hgsAdMarkers' - Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
--
-- * 'hgsKeyFormat' - The value specifies how the key is represented in the resource identified by the URI.  If parameter is absent, an implicit value of "identity" is used.  A reverse DNS string can also be given.
--
-- * 'hgsSegmentLength' - Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
--
-- * 'hgsHlsId3SegmentTagging' - State of HLS ID3 Segment Tagging
--
-- * 'hgsTimedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
--
-- * 'hgsBaseURLContent' - A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- * 'hgsOutputSelection' - MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if applicable, and media manifests) for this output group. VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this output group, but not a master manifest. SEGMENTS_ONLY: Does not generate any manifests for this output group.
--
-- * 'hgsCaptionLanguageSetting' - Applies only to 608 Embedded output captions. insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. none: Include CLOSED-CAPTIONS=NONE line in the manifest. omit: Omit any CLOSED-CAPTIONS line from the manifest.
--
-- * 'hgsSegmentsPerSubdirectory' - Number of segments to write to a subdirectory before starting a new one. directoryStructure must be subdirectoryPerStream for this setting to have an effect.
--
-- * 'hgsManifestDurationFormat' - Indicates whether the output manifest should use floating point or integer values for segment duration.
--
-- * 'hgsIvSource' - For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If this setting is "followsSegmentNumber", it will cause the IV to change every segment (to match the segment number). If this is set to "explicit", you must enter a constantIv value.
--
-- * 'hgsSegmentationMode' - useInputSegmentation has been deprecated. The configured segment size is always used.
--
-- * 'hgsKeyFormatVersions' - Either a single positive integer version value or a slash delimited list of version values (1/2/3).
--
-- * 'hgsClientCache' - When set to "disabled", sets the #EXT-X-ALLOW-CACHE:no tag in the manifest, which prevents clients from saving media segments for later replay.
--
-- * 'hgsTimestampDeltaMilliseconds' - Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- * 'hgsBaseURLManifest1' - Optional. One value per output group. Complete this field only if you are completing Base URL manifest A, and the downstream system has notified you that the child manifest files for pipeline 1 of all outputs are in a location different from the child manifest files for pipeline 0.
--
-- * 'hgsRedundantManifest' - ENABLED: The master manifest (.m3u8 file) for each pipeline includes information about both pipelines: first its own media files, then the media files of the other pipeline. This feature allows playout device that support stale manifest detection to switch from one manifest to the other, when the current manifest seems to be stale. There are still two destinations and two master manifests, but both master manifests reference the media files from both pipelines. DISABLED: The master manifest (.m3u8 file) for each pipeline includes information about its own pipeline only. For an HLS output group with MediaPackage as the destination, the DISABLED behavior is always followed. MediaPackage regenerates the manifests it serves to players so a redundant manifest from MediaLive is irrelevant.
--
-- * 'hgsStreamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
--
-- * 'hgsKeepSegments' - Applies only if Mode field is LIVE. Specifies the number of media segments (.ts files) to retain in the destination directory.
--
-- * 'hgsBaseURLContent1' - Optional. One value per output group. This field is required only if you are completing Base URL content A, and the downstream system has notified you that the media files for pipeline 1 of all outputs are in a location different from the media files for pipeline 0.
--
-- * 'hgsManifestCompression' - When set to gzip, compresses HLS playlist.
--
-- * 'hgsDestination' - A directory or HTTP destination for the HLS segments, manifest files, and encryption keys (if enabled).
hlsGroupSettings
    :: OutputLocationRef -- ^ 'hgsDestination'
    -> HlsGroupSettings
hlsGroupSettings pDestination_
  = HlsGroupSettings'{_hgsDirectoryStructure = Nothing,
                      _hgsEncryptionType = Nothing,
                      _hgsTimedMetadataId3Period = Nothing,
                      _hgsIvInManifest = Nothing, _hgsTsFileMode = Nothing,
                      _hgsMinSegmentLength = Nothing,
                      _hgsIFrameOnlyPlaylists = Nothing,
                      _hgsProgramDateTime = Nothing,
                      _hgsIndexNSegments = Nothing,
                      _hgsProgramDateTimePeriod = Nothing,
                      _hgsCodecSpecification = Nothing,
                      _hgsHlsCdnSettings = Nothing,
                      _hgsCaptionLanguageMappings = Nothing,
                      _hgsInputLossAction = Nothing, _hgsMode = Nothing,
                      _hgsKeyProviderSettings = Nothing,
                      _hgsConstantIv = Nothing,
                      _hgsBaseURLManifest = Nothing,
                      _hgsAdMarkers = Nothing, _hgsKeyFormat = Nothing,
                      _hgsSegmentLength = Nothing,
                      _hgsHlsId3SegmentTagging = Nothing,
                      _hgsTimedMetadataId3Frame = Nothing,
                      _hgsBaseURLContent = Nothing,
                      _hgsOutputSelection = Nothing,
                      _hgsCaptionLanguageSetting = Nothing,
                      _hgsSegmentsPerSubdirectory = Nothing,
                      _hgsManifestDurationFormat = Nothing,
                      _hgsIvSource = Nothing,
                      _hgsSegmentationMode = Nothing,
                      _hgsKeyFormatVersions = Nothing,
                      _hgsClientCache = Nothing,
                      _hgsTimestampDeltaMilliseconds = Nothing,
                      _hgsBaseURLManifest1 = Nothing,
                      _hgsRedundantManifest = Nothing,
                      _hgsStreamInfResolution = Nothing,
                      _hgsKeepSegments = Nothing,
                      _hgsBaseURLContent1 = Nothing,
                      _hgsManifestCompression = Nothing,
                      _hgsDestination = pDestination_}

-- | Place segments in subdirectories.
hgsDirectoryStructure :: Lens' HlsGroupSettings (Maybe HlsDirectoryStructure)
hgsDirectoryStructure = lens _hgsDirectoryStructure (\ s a -> s{_hgsDirectoryStructure = a})

-- | Encrypts the segments with the given encryption scheme.  Exclude this parameter if no encryption is desired.
hgsEncryptionType :: Lens' HlsGroupSettings (Maybe HlsEncryptionType)
hgsEncryptionType = lens _hgsEncryptionType (\ s a -> s{_hgsEncryptionType = a})

-- | Timed Metadata interval in seconds.
hgsTimedMetadataId3Period :: Lens' HlsGroupSettings (Maybe Natural)
hgsTimedMetadataId3Period = lens _hgsTimedMetadataId3Period (\ s a -> s{_hgsTimedMetadataId3Period = a}) . mapping _Nat

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If set to "include", IV is listed in the manifest, otherwise the IV is not in the manifest.
hgsIvInManifest :: Lens' HlsGroupSettings (Maybe HlsIvInManifest)
hgsIvInManifest = lens _hgsIvInManifest (\ s a -> s{_hgsIvInManifest = a})

-- | SEGMENTED_FILES: Emit the program as segments - multiple .ts media files. SINGLE_FILE: Applies only if Mode field is VOD. Emit the program as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE tags to index segments for playback. A typical use for this value is when sending the output to AWS Elemental MediaConvert, which can accept only a single media file. Playback while the channel is running is not guaranteed due to HTTP server caching.
hgsTsFileMode :: Lens' HlsGroupSettings (Maybe HlsTsFileMode)
hgsTsFileMode = lens _hgsTsFileMode (\ s a -> s{_hgsTsFileMode = a})

-- | When set, minimumSegmentLength is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
hgsMinSegmentLength :: Lens' HlsGroupSettings (Maybe Natural)
hgsMinSegmentLength = lens _hgsMinSegmentLength (\ s a -> s{_hgsMinSegmentLength = a}) . mapping _Nat

-- | DISABLED: Do not create an I-frame-only manifest, but do create the master and media manifests (according to the Output Selection field). STANDARD: Create an I-frame-only manifest for each output that contains video, as well as the other manifests (according to the Output Selection field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries identifying the I-frame position. For example, #EXT-X-BYTERANGE:160364@1461888"
hgsIFrameOnlyPlaylists :: Lens' HlsGroupSettings (Maybe IFrameOnlyPlaylistType)
hgsIFrameOnlyPlaylists = lens _hgsIFrameOnlyPlaylists (\ s a -> s{_hgsIFrameOnlyPlaylists = a})

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestampOffset.
hgsProgramDateTime :: Lens' HlsGroupSettings (Maybe HlsProgramDateTime)
hgsProgramDateTime = lens _hgsProgramDateTime (\ s a -> s{_hgsProgramDateTime = a})

-- | Applies only if Mode field is LIVE. Specifies the maximum number of segments in the media manifest file. After this maximum, older segments are removed from the media manifest. This number must be less than or equal to the Keep Segments field.
hgsIndexNSegments :: Lens' HlsGroupSettings (Maybe Natural)
hgsIndexNSegments = lens _hgsIndexNSegments (\ s a -> s{_hgsIndexNSegments = a}) . mapping _Nat

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
hgsProgramDateTimePeriod :: Lens' HlsGroupSettings (Maybe Natural)
hgsProgramDateTimePeriod = lens _hgsProgramDateTimePeriod (\ s a -> s{_hgsProgramDateTimePeriod = a}) . mapping _Nat

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
hgsCodecSpecification :: Lens' HlsGroupSettings (Maybe HlsCodecSpecification)
hgsCodecSpecification = lens _hgsCodecSpecification (\ s a -> s{_hgsCodecSpecification = a})

-- | Parameters that control interactions with the CDN.
hgsHlsCdnSettings :: Lens' HlsGroupSettings (Maybe HlsCdnSettings)
hgsHlsCdnSettings = lens _hgsHlsCdnSettings (\ s a -> s{_hgsHlsCdnSettings = a})

-- | Mapping of up to 4 caption channels to caption languages.  Is only meaningful if captionLanguageSetting is set to "insert".
hgsCaptionLanguageMappings :: Lens' HlsGroupSettings [CaptionLanguageMapping]
hgsCaptionLanguageMappings = lens _hgsCaptionLanguageMappings (\ s a -> s{_hgsCaptionLanguageMappings = a}) . _Default . _Coerce

-- | Parameter that control output group behavior on input loss.
hgsInputLossAction :: Lens' HlsGroupSettings (Maybe InputLossActionForHlsOut)
hgsInputLossAction = lens _hgsInputLossAction (\ s a -> s{_hgsInputLossAction = a})

-- | If "vod", all segments are indexed and kept permanently in the destination and manifest. If "live", only the number segments specified in keepSegments and indexNSegments are kept; newer segments replace older segments, which may prevent players from rewinding all the way to the beginning of the event. VOD mode uses HLS EXT-X-PLAYLIST-TYPE of EVENT while the channel is running, converting it to a "VOD" type manifest on completion of the stream.
hgsMode :: Lens' HlsGroupSettings (Maybe HlsMode)
hgsMode = lens _hgsMode (\ s a -> s{_hgsMode = a})

-- | The key provider settings.
hgsKeyProviderSettings :: Lens' HlsGroupSettings (Maybe KeyProviderSettings)
hgsKeyProviderSettings = lens _hgsKeyProviderSettings (\ s a -> s{_hgsKeyProviderSettings = a})

-- | For use with encryptionType. This is a 128-bit, 16-byte hex value represented by a 32-character text string. If ivSource is set to "explicit" then this parameter is required and is used as the IV for encryption.
hgsConstantIv :: Lens' HlsGroupSettings (Maybe Text)
hgsConstantIv = lens _hgsConstantIv (\ s a -> s{_hgsConstantIv = a})

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
hgsBaseURLManifest :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURLManifest = lens _hgsBaseURLManifest (\ s a -> s{_hgsBaseURLManifest = a})

-- | Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
hgsAdMarkers :: Lens' HlsGroupSettings [HlsAdMarkers]
hgsAdMarkers = lens _hgsAdMarkers (\ s a -> s{_hgsAdMarkers = a}) . _Default . _Coerce

-- | The value specifies how the key is represented in the resource identified by the URI.  If parameter is absent, an implicit value of "identity" is used.  A reverse DNS string can also be given.
hgsKeyFormat :: Lens' HlsGroupSettings (Maybe Text)
hgsKeyFormat = lens _hgsKeyFormat (\ s a -> s{_hgsKeyFormat = a})

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
hgsSegmentLength :: Lens' HlsGroupSettings (Maybe Natural)
hgsSegmentLength = lens _hgsSegmentLength (\ s a -> s{_hgsSegmentLength = a}) . mapping _Nat

-- | State of HLS ID3 Segment Tagging
hgsHlsId3SegmentTagging :: Lens' HlsGroupSettings (Maybe HlsId3SegmentTaggingState)
hgsHlsId3SegmentTagging = lens _hgsHlsId3SegmentTagging (\ s a -> s{_hgsHlsId3SegmentTagging = a})

-- | Indicates ID3 frame that has the timecode.
hgsTimedMetadataId3Frame :: Lens' HlsGroupSettings (Maybe HlsTimedMetadataId3Frame)
hgsTimedMetadataId3Frame = lens _hgsTimedMetadataId3Frame (\ s a -> s{_hgsTimedMetadataId3Frame = a})

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
hgsBaseURLContent :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURLContent = lens _hgsBaseURLContent (\ s a -> s{_hgsBaseURLContent = a})

-- | MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if applicable, and media manifests) for this output group. VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this output group, but not a master manifest. SEGMENTS_ONLY: Does not generate any manifests for this output group.
hgsOutputSelection :: Lens' HlsGroupSettings (Maybe HlsOutputSelection)
hgsOutputSelection = lens _hgsOutputSelection (\ s a -> s{_hgsOutputSelection = a})

-- | Applies only to 608 Embedded output captions. insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. none: Include CLOSED-CAPTIONS=NONE line in the manifest. omit: Omit any CLOSED-CAPTIONS line from the manifest.
hgsCaptionLanguageSetting :: Lens' HlsGroupSettings (Maybe HlsCaptionLanguageSetting)
hgsCaptionLanguageSetting = lens _hgsCaptionLanguageSetting (\ s a -> s{_hgsCaptionLanguageSetting = a})

-- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be subdirectoryPerStream for this setting to have an effect.
hgsSegmentsPerSubdirectory :: Lens' HlsGroupSettings (Maybe Natural)
hgsSegmentsPerSubdirectory = lens _hgsSegmentsPerSubdirectory (\ s a -> s{_hgsSegmentsPerSubdirectory = a}) . mapping _Nat

-- | Indicates whether the output manifest should use floating point or integer values for segment duration.
hgsManifestDurationFormat :: Lens' HlsGroupSettings (Maybe HlsManifestDurationFormat)
hgsManifestDurationFormat = lens _hgsManifestDurationFormat (\ s a -> s{_hgsManifestDurationFormat = a})

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If this setting is "followsSegmentNumber", it will cause the IV to change every segment (to match the segment number). If this is set to "explicit", you must enter a constantIv value.
hgsIvSource :: Lens' HlsGroupSettings (Maybe HlsIvSource)
hgsIvSource = lens _hgsIvSource (\ s a -> s{_hgsIvSource = a})

-- | useInputSegmentation has been deprecated. The configured segment size is always used.
hgsSegmentationMode :: Lens' HlsGroupSettings (Maybe HlsSegmentationMode)
hgsSegmentationMode = lens _hgsSegmentationMode (\ s a -> s{_hgsSegmentationMode = a})

-- | Either a single positive integer version value or a slash delimited list of version values (1/2/3).
hgsKeyFormatVersions :: Lens' HlsGroupSettings (Maybe Text)
hgsKeyFormatVersions = lens _hgsKeyFormatVersions (\ s a -> s{_hgsKeyFormatVersions = a})

-- | When set to "disabled", sets the #EXT-X-ALLOW-CACHE:no tag in the manifest, which prevents clients from saving media segments for later replay.
hgsClientCache :: Lens' HlsGroupSettings (Maybe HlsClientCache)
hgsClientCache = lens _hgsClientCache (\ s a -> s{_hgsClientCache = a})

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
hgsTimestampDeltaMilliseconds :: Lens' HlsGroupSettings (Maybe Natural)
hgsTimestampDeltaMilliseconds = lens _hgsTimestampDeltaMilliseconds (\ s a -> s{_hgsTimestampDeltaMilliseconds = a}) . mapping _Nat

-- | Optional. One value per output group. Complete this field only if you are completing Base URL manifest A, and the downstream system has notified you that the child manifest files for pipeline 1 of all outputs are in a location different from the child manifest files for pipeline 0.
hgsBaseURLManifest1 :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURLManifest1 = lens _hgsBaseURLManifest1 (\ s a -> s{_hgsBaseURLManifest1 = a})

-- | ENABLED: The master manifest (.m3u8 file) for each pipeline includes information about both pipelines: first its own media files, then the media files of the other pipeline. This feature allows playout device that support stale manifest detection to switch from one manifest to the other, when the current manifest seems to be stale. There are still two destinations and two master manifests, but both master manifests reference the media files from both pipelines. DISABLED: The master manifest (.m3u8 file) for each pipeline includes information about its own pipeline only. For an HLS output group with MediaPackage as the destination, the DISABLED behavior is always followed. MediaPackage regenerates the manifests it serves to players so a redundant manifest from MediaLive is irrelevant.
hgsRedundantManifest :: Lens' HlsGroupSettings (Maybe HlsRedundantManifest)
hgsRedundantManifest = lens _hgsRedundantManifest (\ s a -> s{_hgsRedundantManifest = a})

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
hgsStreamInfResolution :: Lens' HlsGroupSettings (Maybe HlsStreamInfResolution)
hgsStreamInfResolution = lens _hgsStreamInfResolution (\ s a -> s{_hgsStreamInfResolution = a})

-- | Applies only if Mode field is LIVE. Specifies the number of media segments (.ts files) to retain in the destination directory.
hgsKeepSegments :: Lens' HlsGroupSettings (Maybe Natural)
hgsKeepSegments = lens _hgsKeepSegments (\ s a -> s{_hgsKeepSegments = a}) . mapping _Nat

-- | Optional. One value per output group. This field is required only if you are completing Base URL content A, and the downstream system has notified you that the media files for pipeline 1 of all outputs are in a location different from the media files for pipeline 0.
hgsBaseURLContent1 :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURLContent1 = lens _hgsBaseURLContent1 (\ s a -> s{_hgsBaseURLContent1 = a})

-- | When set to gzip, compresses HLS playlist.
hgsManifestCompression :: Lens' HlsGroupSettings (Maybe HlsManifestCompression)
hgsManifestCompression = lens _hgsManifestCompression (\ s a -> s{_hgsManifestCompression = a})

-- | A directory or HTTP destination for the HLS segments, manifest files, and encryption keys (if enabled).
hgsDestination :: Lens' HlsGroupSettings OutputLocationRef
hgsDestination = lens _hgsDestination (\ s a -> s{_hgsDestination = a})

instance FromJSON HlsGroupSettings where
        parseJSON
          = withObject "HlsGroupSettings"
              (\ x ->
                 HlsGroupSettings' <$>
                   (x .:? "directoryStructure") <*>
                     (x .:? "encryptionType")
                     <*> (x .:? "timedMetadataId3Period")
                     <*> (x .:? "ivInManifest")
                     <*> (x .:? "tsFileMode")
                     <*> (x .:? "minSegmentLength")
                     <*> (x .:? "iFrameOnlyPlaylists")
                     <*> (x .:? "programDateTime")
                     <*> (x .:? "indexNSegments")
                     <*> (x .:? "programDateTimePeriod")
                     <*> (x .:? "codecSpecification")
                     <*> (x .:? "hlsCdnSettings")
                     <*> (x .:? "captionLanguageMappings" .!= mempty)
                     <*> (x .:? "inputLossAction")
                     <*> (x .:? "mode")
                     <*> (x .:? "keyProviderSettings")
                     <*> (x .:? "constantIv")
                     <*> (x .:? "baseUrlManifest")
                     <*> (x .:? "adMarkers" .!= mempty)
                     <*> (x .:? "keyFormat")
                     <*> (x .:? "segmentLength")
                     <*> (x .:? "hlsId3SegmentTagging")
                     <*> (x .:? "timedMetadataId3Frame")
                     <*> (x .:? "baseUrlContent")
                     <*> (x .:? "outputSelection")
                     <*> (x .:? "captionLanguageSetting")
                     <*> (x .:? "segmentsPerSubdirectory")
                     <*> (x .:? "manifestDurationFormat")
                     <*> (x .:? "ivSource")
                     <*> (x .:? "segmentationMode")
                     <*> (x .:? "keyFormatVersions")
                     <*> (x .:? "clientCache")
                     <*> (x .:? "timestampDeltaMilliseconds")
                     <*> (x .:? "baseUrlManifest1")
                     <*> (x .:? "redundantManifest")
                     <*> (x .:? "streamInfResolution")
                     <*> (x .:? "keepSegments")
                     <*> (x .:? "baseUrlContent1")
                     <*> (x .:? "manifestCompression")
                     <*> (x .: "destination"))

instance Hashable HlsGroupSettings where

instance NFData HlsGroupSettings where

instance ToJSON HlsGroupSettings where
        toJSON HlsGroupSettings'{..}
          = object
              (catMaybes
                 [("directoryStructure" .=) <$>
                    _hgsDirectoryStructure,
                  ("encryptionType" .=) <$> _hgsEncryptionType,
                  ("timedMetadataId3Period" .=) <$>
                    _hgsTimedMetadataId3Period,
                  ("ivInManifest" .=) <$> _hgsIvInManifest,
                  ("tsFileMode" .=) <$> _hgsTsFileMode,
                  ("minSegmentLength" .=) <$> _hgsMinSegmentLength,
                  ("iFrameOnlyPlaylists" .=) <$>
                    _hgsIFrameOnlyPlaylists,
                  ("programDateTime" .=) <$> _hgsProgramDateTime,
                  ("indexNSegments" .=) <$> _hgsIndexNSegments,
                  ("programDateTimePeriod" .=) <$>
                    _hgsProgramDateTimePeriod,
                  ("codecSpecification" .=) <$> _hgsCodecSpecification,
                  ("hlsCdnSettings" .=) <$> _hgsHlsCdnSettings,
                  ("captionLanguageMappings" .=) <$>
                    _hgsCaptionLanguageMappings,
                  ("inputLossAction" .=) <$> _hgsInputLossAction,
                  ("mode" .=) <$> _hgsMode,
                  ("keyProviderSettings" .=) <$>
                    _hgsKeyProviderSettings,
                  ("constantIv" .=) <$> _hgsConstantIv,
                  ("baseUrlManifest" .=) <$> _hgsBaseURLManifest,
                  ("adMarkers" .=) <$> _hgsAdMarkers,
                  ("keyFormat" .=) <$> _hgsKeyFormat,
                  ("segmentLength" .=) <$> _hgsSegmentLength,
                  ("hlsId3SegmentTagging" .=) <$>
                    _hgsHlsId3SegmentTagging,
                  ("timedMetadataId3Frame" .=) <$>
                    _hgsTimedMetadataId3Frame,
                  ("baseUrlContent" .=) <$> _hgsBaseURLContent,
                  ("outputSelection" .=) <$> _hgsOutputSelection,
                  ("captionLanguageSetting" .=) <$>
                    _hgsCaptionLanguageSetting,
                  ("segmentsPerSubdirectory" .=) <$>
                    _hgsSegmentsPerSubdirectory,
                  ("manifestDurationFormat" .=) <$>
                    _hgsManifestDurationFormat,
                  ("ivSource" .=) <$> _hgsIvSource,
                  ("segmentationMode" .=) <$> _hgsSegmentationMode,
                  ("keyFormatVersions" .=) <$> _hgsKeyFormatVersions,
                  ("clientCache" .=) <$> _hgsClientCache,
                  ("timestampDeltaMilliseconds" .=) <$>
                    _hgsTimestampDeltaMilliseconds,
                  ("baseUrlManifest1" .=) <$> _hgsBaseURLManifest1,
                  ("redundantManifest" .=) <$> _hgsRedundantManifest,
                  ("streamInfResolution" .=) <$>
                    _hgsStreamInfResolution,
                  ("keepSegments" .=) <$> _hgsKeepSegments,
                  ("baseUrlContent1" .=) <$> _hgsBaseURLContent1,
                  ("manifestCompression" .=) <$>
                    _hgsManifestCompression,
                  Just ("destination" .= _hgsDestination)])

-- | Settings for the action to insert a user-defined ID3 tag in each HLS segment
--
-- /See:/ 'hlsId3SegmentTaggingScheduleActionSettings' smart constructor.
newtype HlsId3SegmentTaggingScheduleActionSettings = HlsId3SegmentTaggingScheduleActionSettings'{_histsasTag
                                                                                                 ::
                                                                                                 Text}
                                                       deriving (Eq, Read, Show,
                                                                 Data, Typeable,
                                                                 Generic)

-- | Creates a value of 'HlsId3SegmentTaggingScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'histsasTag' - ID3 tag to insert into each segment. Supports special keyword identifiers to substitute in segment-related values.\nSupported keyword identifiers: https://docs.aws.amazon.com/medialive/latest/ug/variable-data-identifiers.html
hlsId3SegmentTaggingScheduleActionSettings
    :: Text -- ^ 'histsasTag'
    -> HlsId3SegmentTaggingScheduleActionSettings
hlsId3SegmentTaggingScheduleActionSettings pTag_
  = HlsId3SegmentTaggingScheduleActionSettings'{_histsasTag
                                                  = pTag_}

-- | ID3 tag to insert into each segment. Supports special keyword identifiers to substitute in segment-related values.\nSupported keyword identifiers: https://docs.aws.amazon.com/medialive/latest/ug/variable-data-identifiers.html
histsasTag :: Lens' HlsId3SegmentTaggingScheduleActionSettings Text
histsasTag = lens _histsasTag (\ s a -> s{_histsasTag = a})

instance FromJSON
           HlsId3SegmentTaggingScheduleActionSettings
         where
        parseJSON
          = withObject
              "HlsId3SegmentTaggingScheduleActionSettings"
              (\ x ->
                 HlsId3SegmentTaggingScheduleActionSettings' <$>
                   (x .: "tag"))

instance Hashable
           HlsId3SegmentTaggingScheduleActionSettings
         where

instance NFData
           HlsId3SegmentTaggingScheduleActionSettings
         where

instance ToJSON
           HlsId3SegmentTaggingScheduleActionSettings
         where
        toJSON
          HlsId3SegmentTaggingScheduleActionSettings'{..}
          = object (catMaybes [Just ("tag" .= _histsasTag)])

-- | Hls Input Settings
--
-- /See:/ 'hlsInputSettings' smart constructor.
data HlsInputSettings = HlsInputSettings'{_hisBufferSegments
                                          :: !(Maybe Nat),
                                          _hisRetries :: !(Maybe Nat),
                                          _hisRetryInterval :: !(Maybe Nat),
                                          _hisBandwidth :: !(Maybe Nat)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsInputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hisBufferSegments' - When specified, reading of the HLS input will begin this many buffer segments from the end (most recently written segment).  When not specified, the HLS input will begin with the first segment specified in the m3u8.
--
-- * 'hisRetries' - The number of consecutive times that attempts to read a manifest or segment must fail before the input is considered unavailable.
--
-- * 'hisRetryInterval' - The number of seconds between retries when an attempt to read a manifest or segment fails.
--
-- * 'hisBandwidth' - When specified the HLS stream with the m3u8 BANDWIDTH that most closely matches this value will be chosen, otherwise the highest bandwidth stream in the m3u8 will be chosen.  The bitrate is specified in bits per second, as in an HLS manifest.
hlsInputSettings
    :: HlsInputSettings
hlsInputSettings
  = HlsInputSettings'{_hisBufferSegments = Nothing,
                      _hisRetries = Nothing, _hisRetryInterval = Nothing,
                      _hisBandwidth = Nothing}

-- | When specified, reading of the HLS input will begin this many buffer segments from the end (most recently written segment).  When not specified, the HLS input will begin with the first segment specified in the m3u8.
hisBufferSegments :: Lens' HlsInputSettings (Maybe Natural)
hisBufferSegments = lens _hisBufferSegments (\ s a -> s{_hisBufferSegments = a}) . mapping _Nat

-- | The number of consecutive times that attempts to read a manifest or segment must fail before the input is considered unavailable.
hisRetries :: Lens' HlsInputSettings (Maybe Natural)
hisRetries = lens _hisRetries (\ s a -> s{_hisRetries = a}) . mapping _Nat

-- | The number of seconds between retries when an attempt to read a manifest or segment fails.
hisRetryInterval :: Lens' HlsInputSettings (Maybe Natural)
hisRetryInterval = lens _hisRetryInterval (\ s a -> s{_hisRetryInterval = a}) . mapping _Nat

-- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely matches this value will be chosen, otherwise the highest bandwidth stream in the m3u8 will be chosen.  The bitrate is specified in bits per second, as in an HLS manifest.
hisBandwidth :: Lens' HlsInputSettings (Maybe Natural)
hisBandwidth = lens _hisBandwidth (\ s a -> s{_hisBandwidth = a}) . mapping _Nat

instance FromJSON HlsInputSettings where
        parseJSON
          = withObject "HlsInputSettings"
              (\ x ->
                 HlsInputSettings' <$>
                   (x .:? "bufferSegments") <*> (x .:? "retries") <*>
                     (x .:? "retryInterval")
                     <*> (x .:? "bandwidth"))

instance Hashable HlsInputSettings where

instance NFData HlsInputSettings where

instance ToJSON HlsInputSettings where
        toJSON HlsInputSettings'{..}
          = object
              (catMaybes
                 [("bufferSegments" .=) <$> _hisBufferSegments,
                  ("retries" .=) <$> _hisRetries,
                  ("retryInterval" .=) <$> _hisRetryInterval,
                  ("bandwidth" .=) <$> _hisBandwidth])

-- | Hls Media Store Settings
--
-- /See:/ 'hlsMediaStoreSettings' smart constructor.
data HlsMediaStoreSettings = HlsMediaStoreSettings'{_hmssNumRetries
                                                    :: !(Maybe Nat),
                                                    _hmssConnectionRetryInterval
                                                    :: !(Maybe Nat),
                                                    _hmssFilecacheDuration ::
                                                    !(Maybe Nat),
                                                    _hmssMediaStoreStorageClass
                                                    ::
                                                    !(Maybe
                                                        HlsMediaStoreStorageClass),
                                                    _hmssRestartDelay ::
                                                    !(Maybe Nat)}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'HlsMediaStoreSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmssNumRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- * 'hmssConnectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- * 'hmssFilecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- * 'hmssMediaStoreStorageClass' - When set to temporal, output files are stored in non-persistent memory for faster reading and writing.
--
-- * 'hmssRestartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hlsMediaStoreSettings
    :: HlsMediaStoreSettings
hlsMediaStoreSettings
  = HlsMediaStoreSettings'{_hmssNumRetries = Nothing,
                           _hmssConnectionRetryInterval = Nothing,
                           _hmssFilecacheDuration = Nothing,
                           _hmssMediaStoreStorageClass = Nothing,
                           _hmssRestartDelay = Nothing}

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
hmssNumRetries :: Lens' HlsMediaStoreSettings (Maybe Natural)
hmssNumRetries = lens _hmssNumRetries (\ s a -> s{_hmssNumRetries = a}) . mapping _Nat

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
hmssConnectionRetryInterval :: Lens' HlsMediaStoreSettings (Maybe Natural)
hmssConnectionRetryInterval = lens _hmssConnectionRetryInterval (\ s a -> s{_hmssConnectionRetryInterval = a}) . mapping _Nat

-- | Size in seconds of file cache for streaming outputs.
hmssFilecacheDuration :: Lens' HlsMediaStoreSettings (Maybe Natural)
hmssFilecacheDuration = lens _hmssFilecacheDuration (\ s a -> s{_hmssFilecacheDuration = a}) . mapping _Nat

-- | When set to temporal, output files are stored in non-persistent memory for faster reading and writing.
hmssMediaStoreStorageClass :: Lens' HlsMediaStoreSettings (Maybe HlsMediaStoreStorageClass)
hmssMediaStoreStorageClass = lens _hmssMediaStoreStorageClass (\ s a -> s{_hmssMediaStoreStorageClass = a})

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hmssRestartDelay :: Lens' HlsMediaStoreSettings (Maybe Natural)
hmssRestartDelay = lens _hmssRestartDelay (\ s a -> s{_hmssRestartDelay = a}) . mapping _Nat

instance FromJSON HlsMediaStoreSettings where
        parseJSON
          = withObject "HlsMediaStoreSettings"
              (\ x ->
                 HlsMediaStoreSettings' <$>
                   (x .:? "numRetries") <*>
                     (x .:? "connectionRetryInterval")
                     <*> (x .:? "filecacheDuration")
                     <*> (x .:? "mediaStoreStorageClass")
                     <*> (x .:? "restartDelay"))

instance Hashable HlsMediaStoreSettings where

instance NFData HlsMediaStoreSettings where

instance ToJSON HlsMediaStoreSettings where
        toJSON HlsMediaStoreSettings'{..}
          = object
              (catMaybes
                 [("numRetries" .=) <$> _hmssNumRetries,
                  ("connectionRetryInterval" .=) <$>
                    _hmssConnectionRetryInterval,
                  ("filecacheDuration" .=) <$> _hmssFilecacheDuration,
                  ("mediaStoreStorageClass" .=) <$>
                    _hmssMediaStoreStorageClass,
                  ("restartDelay" .=) <$> _hmssRestartDelay])

-- | Hls Output Settings
--
-- /See:/ 'hlsOutputSettings' smart constructor.
data HlsOutputSettings = HlsOutputSettings'{_hosH265PackagingType
                                            :: !(Maybe HlsH265PackagingType),
                                            _hosSegmentModifier ::
                                            !(Maybe Text),
                                            _hosNameModifier :: !(Maybe Text),
                                            _hosHlsSettings :: !HlsSettings}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hosH265PackagingType' - Only applicable when this output is referencing an H.265 video description. Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
--
-- * 'hosSegmentModifier' - String concatenated to end of segment filenames.
--
-- * 'hosNameModifier' - String concatenated to the end of the destination filename. Accepts \"Format Identifiers\":#formatIdentifierParameters.
--
-- * 'hosHlsSettings' - Settings regarding the underlying stream. These settings are different for audio-only outputs.
hlsOutputSettings
    :: HlsSettings -- ^ 'hosHlsSettings'
    -> HlsOutputSettings
hlsOutputSettings pHlsSettings_
  = HlsOutputSettings'{_hosH265PackagingType = Nothing,
                       _hosSegmentModifier = Nothing,
                       _hosNameModifier = Nothing,
                       _hosHlsSettings = pHlsSettings_}

-- | Only applicable when this output is referencing an H.265 video description. Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
hosH265PackagingType :: Lens' HlsOutputSettings (Maybe HlsH265PackagingType)
hosH265PackagingType = lens _hosH265PackagingType (\ s a -> s{_hosH265PackagingType = a})

-- | String concatenated to end of segment filenames.
hosSegmentModifier :: Lens' HlsOutputSettings (Maybe Text)
hosSegmentModifier = lens _hosSegmentModifier (\ s a -> s{_hosSegmentModifier = a})

-- | String concatenated to the end of the destination filename. Accepts \"Format Identifiers\":#formatIdentifierParameters.
hosNameModifier :: Lens' HlsOutputSettings (Maybe Text)
hosNameModifier = lens _hosNameModifier (\ s a -> s{_hosNameModifier = a})

-- | Settings regarding the underlying stream. These settings are different for audio-only outputs.
hosHlsSettings :: Lens' HlsOutputSettings HlsSettings
hosHlsSettings = lens _hosHlsSettings (\ s a -> s{_hosHlsSettings = a})

instance FromJSON HlsOutputSettings where
        parseJSON
          = withObject "HlsOutputSettings"
              (\ x ->
                 HlsOutputSettings' <$>
                   (x .:? "h265PackagingType") <*>
                     (x .:? "segmentModifier")
                     <*> (x .:? "nameModifier")
                     <*> (x .: "hlsSettings"))

instance Hashable HlsOutputSettings where

instance NFData HlsOutputSettings where

instance ToJSON HlsOutputSettings where
        toJSON HlsOutputSettings'{..}
          = object
              (catMaybes
                 [("h265PackagingType" .=) <$> _hosH265PackagingType,
                  ("segmentModifier" .=) <$> _hosSegmentModifier,
                  ("nameModifier" .=) <$> _hosNameModifier,
                  Just ("hlsSettings" .= _hosHlsSettings)])

-- | Hls Settings
--
-- /See:/ 'hlsSettings' smart constructor.
data HlsSettings = HlsSettings'{_hsFmp4HlsSettings ::
                                !(Maybe Fmp4HlsSettings),
                                _hsAudioOnlyHlsSettings ::
                                !(Maybe AudioOnlyHlsSettings),
                                _hsStandardHlsSettings ::
                                !(Maybe StandardHlsSettings)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsFmp4HlsSettings' - Undocumented member.
--
-- * 'hsAudioOnlyHlsSettings' - Undocumented member.
--
-- * 'hsStandardHlsSettings' - Undocumented member.
hlsSettings
    :: HlsSettings
hlsSettings
  = HlsSettings'{_hsFmp4HlsSettings = Nothing,
                 _hsAudioOnlyHlsSettings = Nothing,
                 _hsStandardHlsSettings = Nothing}

-- | Undocumented member.
hsFmp4HlsSettings :: Lens' HlsSettings (Maybe Fmp4HlsSettings)
hsFmp4HlsSettings = lens _hsFmp4HlsSettings (\ s a -> s{_hsFmp4HlsSettings = a})

-- | Undocumented member.
hsAudioOnlyHlsSettings :: Lens' HlsSettings (Maybe AudioOnlyHlsSettings)
hsAudioOnlyHlsSettings = lens _hsAudioOnlyHlsSettings (\ s a -> s{_hsAudioOnlyHlsSettings = a})

-- | Undocumented member.
hsStandardHlsSettings :: Lens' HlsSettings (Maybe StandardHlsSettings)
hsStandardHlsSettings = lens _hsStandardHlsSettings (\ s a -> s{_hsStandardHlsSettings = a})

instance FromJSON HlsSettings where
        parseJSON
          = withObject "HlsSettings"
              (\ x ->
                 HlsSettings' <$>
                   (x .:? "fmp4HlsSettings") <*>
                     (x .:? "audioOnlyHlsSettings")
                     <*> (x .:? "standardHlsSettings"))

instance Hashable HlsSettings where

instance NFData HlsSettings where

instance ToJSON HlsSettings where
        toJSON HlsSettings'{..}
          = object
              (catMaybes
                 [("fmp4HlsSettings" .=) <$> _hsFmp4HlsSettings,
                  ("audioOnlyHlsSettings" .=) <$>
                    _hsAudioOnlyHlsSettings,
                  ("standardHlsSettings" .=) <$>
                    _hsStandardHlsSettings])

-- | Settings for the action to emit HLS metadata
--
-- /See:/ 'hlsTimedMetadataScheduleActionSettings' smart constructor.
newtype HlsTimedMetadataScheduleActionSettings = HlsTimedMetadataScheduleActionSettings'{_htmsasId3
                                                                                         ::
                                                                                         Text}
                                                   deriving (Eq, Read, Show,
                                                             Data, Typeable,
                                                             Generic)

-- | Creates a value of 'HlsTimedMetadataScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'htmsasId3' - Base64 string formatted according to the ID3 specification: http://id3.org/id3v2.4.0-structure
hlsTimedMetadataScheduleActionSettings
    :: Text -- ^ 'htmsasId3'
    -> HlsTimedMetadataScheduleActionSettings
hlsTimedMetadataScheduleActionSettings pId3_
  = HlsTimedMetadataScheduleActionSettings'{_htmsasId3
                                              = pId3_}

-- | Base64 string formatted according to the ID3 specification: http://id3.org/id3v2.4.0-structure
htmsasId3 :: Lens' HlsTimedMetadataScheduleActionSettings Text
htmsasId3 = lens _htmsasId3 (\ s a -> s{_htmsasId3 = a})

instance FromJSON
           HlsTimedMetadataScheduleActionSettings
         where
        parseJSON
          = withObject "HlsTimedMetadataScheduleActionSettings"
              (\ x ->
                 HlsTimedMetadataScheduleActionSettings' <$>
                   (x .: "id3"))

instance Hashable
           HlsTimedMetadataScheduleActionSettings
         where

instance NFData
           HlsTimedMetadataScheduleActionSettings
         where

instance ToJSON
           HlsTimedMetadataScheduleActionSettings
         where
        toJSON HlsTimedMetadataScheduleActionSettings'{..}
          = object (catMaybes [Just ("id3" .= _htmsasId3)])

-- | Hls Webdav Settings
--
-- /See:/ 'hlsWebdavSettings' smart constructor.
data HlsWebdavSettings = HlsWebdavSettings'{_hwsHTTPTransferMode
                                            ::
                                            !(Maybe HlsWebdavHTTPTransferMode),
                                            _hwsNumRetries :: !(Maybe Nat),
                                            _hwsConnectionRetryInterval ::
                                            !(Maybe Nat),
                                            _hwsFilecacheDuration ::
                                            !(Maybe Nat),
                                            _hwsRestartDelay :: !(Maybe Nat)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsWebdavSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hwsHTTPTransferMode' - Specify whether or not to use chunked transfer encoding to WebDAV.
--
-- * 'hwsNumRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- * 'hwsConnectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- * 'hwsFilecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- * 'hwsRestartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hlsWebdavSettings
    :: HlsWebdavSettings
hlsWebdavSettings
  = HlsWebdavSettings'{_hwsHTTPTransferMode = Nothing,
                       _hwsNumRetries = Nothing,
                       _hwsConnectionRetryInterval = Nothing,
                       _hwsFilecacheDuration = Nothing,
                       _hwsRestartDelay = Nothing}

-- | Specify whether or not to use chunked transfer encoding to WebDAV.
hwsHTTPTransferMode :: Lens' HlsWebdavSettings (Maybe HlsWebdavHTTPTransferMode)
hwsHTTPTransferMode = lens _hwsHTTPTransferMode (\ s a -> s{_hwsHTTPTransferMode = a})

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
hwsNumRetries :: Lens' HlsWebdavSettings (Maybe Natural)
hwsNumRetries = lens _hwsNumRetries (\ s a -> s{_hwsNumRetries = a}) . mapping _Nat

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
hwsConnectionRetryInterval :: Lens' HlsWebdavSettings (Maybe Natural)
hwsConnectionRetryInterval = lens _hwsConnectionRetryInterval (\ s a -> s{_hwsConnectionRetryInterval = a}) . mapping _Nat

-- | Size in seconds of file cache for streaming outputs.
hwsFilecacheDuration :: Lens' HlsWebdavSettings (Maybe Natural)
hwsFilecacheDuration = lens _hwsFilecacheDuration (\ s a -> s{_hwsFilecacheDuration = a}) . mapping _Nat

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
hwsRestartDelay :: Lens' HlsWebdavSettings (Maybe Natural)
hwsRestartDelay = lens _hwsRestartDelay (\ s a -> s{_hwsRestartDelay = a}) . mapping _Nat

instance FromJSON HlsWebdavSettings where
        parseJSON
          = withObject "HlsWebdavSettings"
              (\ x ->
                 HlsWebdavSettings' <$>
                   (x .:? "httpTransferMode") <*> (x .:? "numRetries")
                     <*> (x .:? "connectionRetryInterval")
                     <*> (x .:? "filecacheDuration")
                     <*> (x .:? "restartDelay"))

instance Hashable HlsWebdavSettings where

instance NFData HlsWebdavSettings where

instance ToJSON HlsWebdavSettings where
        toJSON HlsWebdavSettings'{..}
          = object
              (catMaybes
                 [("httpTransferMode" .=) <$> _hwsHTTPTransferMode,
                  ("numRetries" .=) <$> _hwsNumRetries,
                  ("connectionRetryInterval" .=) <$>
                    _hwsConnectionRetryInterval,
                  ("filecacheDuration" .=) <$> _hwsFilecacheDuration,
                  ("restartDelay" .=) <$> _hwsRestartDelay])

-- | Settings to configure an action so that it occurs as soon as possible.
--
-- /See:/ 'immediateModeScheduleActionStartSettings' smart constructor.
data ImmediateModeScheduleActionStartSettings = ImmediateModeScheduleActionStartSettings'
                                                  deriving (Eq, Read, Show,
                                                            Data, Typeable,
                                                            Generic)

-- | Creates a value of 'ImmediateModeScheduleActionStartSettings' with the minimum fields required to make a request.
--
immediateModeScheduleActionStartSettings
    :: ImmediateModeScheduleActionStartSettings
immediateModeScheduleActionStartSettings
  = ImmediateModeScheduleActionStartSettings'

instance FromJSON
           ImmediateModeScheduleActionStartSettings
         where
        parseJSON
          = withObject
              "ImmediateModeScheduleActionStartSettings"
              (\ x ->
                 pure ImmediateModeScheduleActionStartSettings')

instance Hashable
           ImmediateModeScheduleActionStartSettings
         where

instance NFData
           ImmediateModeScheduleActionStartSettings
         where

instance ToJSON
           ImmediateModeScheduleActionStartSettings
         where
        toJSON = const (Object mempty)

-- | Placeholder documentation for Input
--
-- /See:/ 'input' smart constructor.
data Input = Input'{_iState :: !(Maybe InputState),
                    _iSecurityGroups :: !(Maybe [Text]),
                    _iARN :: !(Maybe Text),
                    _iInputDevices :: !(Maybe [InputDeviceSettings]),
                    _iSources :: !(Maybe [InputSource]),
                    _iDestinations :: !(Maybe [InputDestination]),
                    _iName :: !(Maybe Text),
                    _iAttachedChannels :: !(Maybe [Text]),
                    _iId :: !(Maybe Text),
                    _iInputClass :: !(Maybe InputClass),
                    _iType :: !(Maybe InputType),
                    _iMediaConnectFlows :: !(Maybe [MediaConnectFlow]),
                    _iInputSourceType :: !(Maybe InputSourceType),
                    _iTags :: !(Maybe (Map Text Text)),
                    _iRoleARN :: !(Maybe Text)}
               deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Input' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iState' - Undocumented member.
--
-- * 'iSecurityGroups' - A list of IDs for all the Input Security Groups attached to the input.
--
-- * 'iARN' - The Unique ARN of the input (generated, immutable).
--
-- * 'iInputDevices' - Settings for the input devices.
--
-- * 'iSources' - A list of the sources of the input (PULL-type).
--
-- * 'iDestinations' - A list of the destinations of the input (PUSH-type).
--
-- * 'iName' - The user-assigned name (This is a mutable value).
--
-- * 'iAttachedChannels' - A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
--
-- * 'iId' - The generated ID of the input (unique for user account, immutable).
--
-- * 'iInputClass' - STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails. SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
--
-- * 'iType' - Undocumented member.
--
-- * 'iMediaConnectFlows' - A list of MediaConnect Flows for this input.
--
-- * 'iInputSourceType' - Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
--
-- * 'iTags' - A collection of key-value pairs.
--
-- * 'iRoleARN' - The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
input
    :: Input
input
  = Input'{_iState = Nothing,
           _iSecurityGroups = Nothing, _iARN = Nothing,
           _iInputDevices = Nothing, _iSources = Nothing,
           _iDestinations = Nothing, _iName = Nothing,
           _iAttachedChannels = Nothing, _iId = Nothing,
           _iInputClass = Nothing, _iType = Nothing,
           _iMediaConnectFlows = Nothing,
           _iInputSourceType = Nothing, _iTags = Nothing,
           _iRoleARN = Nothing}

-- | Undocumented member.
iState :: Lens' Input (Maybe InputState)
iState = lens _iState (\ s a -> s{_iState = a})

-- | A list of IDs for all the Input Security Groups attached to the input.
iSecurityGroups :: Lens' Input [Text]
iSecurityGroups = lens _iSecurityGroups (\ s a -> s{_iSecurityGroups = a}) . _Default . _Coerce

-- | The Unique ARN of the input (generated, immutable).
iARN :: Lens' Input (Maybe Text)
iARN = lens _iARN (\ s a -> s{_iARN = a})

-- | Settings for the input devices.
iInputDevices :: Lens' Input [InputDeviceSettings]
iInputDevices = lens _iInputDevices (\ s a -> s{_iInputDevices = a}) . _Default . _Coerce

-- | A list of the sources of the input (PULL-type).
iSources :: Lens' Input [InputSource]
iSources = lens _iSources (\ s a -> s{_iSources = a}) . _Default . _Coerce

-- | A list of the destinations of the input (PUSH-type).
iDestinations :: Lens' Input [InputDestination]
iDestinations = lens _iDestinations (\ s a -> s{_iDestinations = a}) . _Default . _Coerce

-- | The user-assigned name (This is a mutable value).
iName :: Lens' Input (Maybe Text)
iName = lens _iName (\ s a -> s{_iName = a})

-- | A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
iAttachedChannels :: Lens' Input [Text]
iAttachedChannels = lens _iAttachedChannels (\ s a -> s{_iAttachedChannels = a}) . _Default . _Coerce

-- | The generated ID of the input (unique for user account, immutable).
iId :: Lens' Input (Maybe Text)
iId = lens _iId (\ s a -> s{_iId = a})

-- | STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails. SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
iInputClass :: Lens' Input (Maybe InputClass)
iInputClass = lens _iInputClass (\ s a -> s{_iInputClass = a})

-- | Undocumented member.
iType :: Lens' Input (Maybe InputType)
iType = lens _iType (\ s a -> s{_iType = a})

-- | A list of MediaConnect Flows for this input.
iMediaConnectFlows :: Lens' Input [MediaConnectFlow]
iMediaConnectFlows = lens _iMediaConnectFlows (\ s a -> s{_iMediaConnectFlows = a}) . _Default . _Coerce

-- | Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
iInputSourceType :: Lens' Input (Maybe InputSourceType)
iInputSourceType = lens _iInputSourceType (\ s a -> s{_iInputSourceType = a})

-- | A collection of key-value pairs.
iTags :: Lens' Input (HashMap Text Text)
iTags = lens _iTags (\ s a -> s{_iTags = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
iRoleARN :: Lens' Input (Maybe Text)
iRoleARN = lens _iRoleARN (\ s a -> s{_iRoleARN = a})

instance FromJSON Input where
        parseJSON
          = withObject "Input"
              (\ x ->
                 Input' <$>
                   (x .:? "state") <*>
                     (x .:? "securityGroups" .!= mempty)
                     <*> (x .:? "arn")
                     <*> (x .:? "inputDevices" .!= mempty)
                     <*> (x .:? "sources" .!= mempty)
                     <*> (x .:? "destinations" .!= mempty)
                     <*> (x .:? "name")
                     <*> (x .:? "attachedChannels" .!= mempty)
                     <*> (x .:? "id")
                     <*> (x .:? "inputClass")
                     <*> (x .:? "type")
                     <*> (x .:? "mediaConnectFlows" .!= mempty)
                     <*> (x .:? "inputSourceType")
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .:? "roleArn"))

instance Hashable Input where

instance NFData Input where

-- | Placeholder documentation for InputAttachment
--
-- /See:/ 'inputAttachment' smart constructor.
data InputAttachment = InputAttachment'{_iaInputAttachmentName
                                        :: !(Maybe Text),
                                        _iaInputId :: !(Maybe Text),
                                        _iaAutomaticInputFailoverSettings ::
                                        !(Maybe AutomaticInputFailoverSettings),
                                        _iaInputSettings ::
                                        !(Maybe InputSettings)}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaInputAttachmentName' - User-specified name for the attachment. This is required if the user wants to use this input in an input switch action.
--
-- * 'iaInputId' - The ID of the input
--
-- * 'iaAutomaticInputFailoverSettings' - User-specified settings for defining what the conditions are for declaring the input unhealthy and failing over to a different input.
--
-- * 'iaInputSettings' - Settings of an input (caption selector, etc.)
inputAttachment
    :: InputAttachment
inputAttachment
  = InputAttachment'{_iaInputAttachmentName = Nothing,
                     _iaInputId = Nothing,
                     _iaAutomaticInputFailoverSettings = Nothing,
                     _iaInputSettings = Nothing}

-- | User-specified name for the attachment. This is required if the user wants to use this input in an input switch action.
iaInputAttachmentName :: Lens' InputAttachment (Maybe Text)
iaInputAttachmentName = lens _iaInputAttachmentName (\ s a -> s{_iaInputAttachmentName = a})

-- | The ID of the input
iaInputId :: Lens' InputAttachment (Maybe Text)
iaInputId = lens _iaInputId (\ s a -> s{_iaInputId = a})

-- | User-specified settings for defining what the conditions are for declaring the input unhealthy and failing over to a different input.
iaAutomaticInputFailoverSettings :: Lens' InputAttachment (Maybe AutomaticInputFailoverSettings)
iaAutomaticInputFailoverSettings = lens _iaAutomaticInputFailoverSettings (\ s a -> s{_iaAutomaticInputFailoverSettings = a})

-- | Settings of an input (caption selector, etc.)
iaInputSettings :: Lens' InputAttachment (Maybe InputSettings)
iaInputSettings = lens _iaInputSettings (\ s a -> s{_iaInputSettings = a})

instance FromJSON InputAttachment where
        parseJSON
          = withObject "InputAttachment"
              (\ x ->
                 InputAttachment' <$>
                   (x .:? "inputAttachmentName") <*> (x .:? "inputId")
                     <*> (x .:? "automaticInputFailoverSettings")
                     <*> (x .:? "inputSettings"))

instance Hashable InputAttachment where

instance NFData InputAttachment where

instance ToJSON InputAttachment where
        toJSON InputAttachment'{..}
          = object
              (catMaybes
                 [("inputAttachmentName" .=) <$>
                    _iaInputAttachmentName,
                  ("inputId" .=) <$> _iaInputId,
                  ("automaticInputFailoverSettings" .=) <$>
                    _iaAutomaticInputFailoverSettings,
                  ("inputSettings" .=) <$> _iaInputSettings])

-- | Input Channel Level
--
-- /See:/ 'inputChannelLevel' smart constructor.
data InputChannelLevel = InputChannelLevel'{_iclInputChannel
                                            :: !Nat,
                                            _iclGain :: !Int}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputChannelLevel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iclInputChannel' - The index of the input channel used as a source.
--
-- * 'iclGain' - Remixing value. Units are in dB and acceptable values are within the range from -60 (mute) and 6 dB.
inputChannelLevel
    :: Natural -- ^ 'iclInputChannel'
    -> Int -- ^ 'iclGain'
    -> InputChannelLevel
inputChannelLevel pInputChannel_ pGain_
  = InputChannelLevel'{_iclInputChannel =
                         _Nat # pInputChannel_,
                       _iclGain = pGain_}

-- | The index of the input channel used as a source.
iclInputChannel :: Lens' InputChannelLevel Natural
iclInputChannel = lens _iclInputChannel (\ s a -> s{_iclInputChannel = a}) . _Nat

-- | Remixing value. Units are in dB and acceptable values are within the range from -60 (mute) and 6 dB.
iclGain :: Lens' InputChannelLevel Int
iclGain = lens _iclGain (\ s a -> s{_iclGain = a})

instance FromJSON InputChannelLevel where
        parseJSON
          = withObject "InputChannelLevel"
              (\ x ->
                 InputChannelLevel' <$>
                   (x .: "inputChannel") <*> (x .: "gain"))

instance Hashable InputChannelLevel where

instance NFData InputChannelLevel where

instance ToJSON InputChannelLevel where
        toJSON InputChannelLevel'{..}
          = object
              (catMaybes
                 [Just ("inputChannel" .= _iclInputChannel),
                  Just ("gain" .= _iclGain)])

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- /See:/ 'inputClippingSettings' smart constructor.
data InputClippingSettings = InputClippingSettings'{_icsStopTimecode
                                                    :: !(Maybe StopTimecode),
                                                    _icsStartTimecode ::
                                                    !(Maybe StartTimecode),
                                                    _icsInputTimecodeSource ::
                                                    !InputTimecodeSource}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'InputClippingSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icsStopTimecode' - Settings to identify the end of the clip.
--
-- * 'icsStartTimecode' - Settings to identify the start of the clip.
--
-- * 'icsInputTimecodeSource' - The source of the timecodes in the source being clipped.
inputClippingSettings
    :: InputTimecodeSource -- ^ 'icsInputTimecodeSource'
    -> InputClippingSettings
inputClippingSettings pInputTimecodeSource_
  = InputClippingSettings'{_icsStopTimecode = Nothing,
                           _icsStartTimecode = Nothing,
                           _icsInputTimecodeSource = pInputTimecodeSource_}

-- | Settings to identify the end of the clip.
icsStopTimecode :: Lens' InputClippingSettings (Maybe StopTimecode)
icsStopTimecode = lens _icsStopTimecode (\ s a -> s{_icsStopTimecode = a})

-- | Settings to identify the start of the clip.
icsStartTimecode :: Lens' InputClippingSettings (Maybe StartTimecode)
icsStartTimecode = lens _icsStartTimecode (\ s a -> s{_icsStartTimecode = a})

-- | The source of the timecodes in the source being clipped.
icsInputTimecodeSource :: Lens' InputClippingSettings InputTimecodeSource
icsInputTimecodeSource = lens _icsInputTimecodeSource (\ s a -> s{_icsInputTimecodeSource = a})

instance FromJSON InputClippingSettings where
        parseJSON
          = withObject "InputClippingSettings"
              (\ x ->
                 InputClippingSettings' <$>
                   (x .:? "stopTimecode") <*> (x .:? "startTimecode")
                     <*> (x .: "inputTimecodeSource"))

instance Hashable InputClippingSettings where

instance NFData InputClippingSettings where

instance ToJSON InputClippingSettings where
        toJSON InputClippingSettings'{..}
          = object
              (catMaybes
                 [("stopTimecode" .=) <$> _icsStopTimecode,
                  ("startTimecode" .=) <$> _icsStartTimecode,
                  Just
                    ("inputTimecodeSource" .= _icsInputTimecodeSource)])

-- | The settings for a PUSH type input.
--
-- /See:/ 'inputDestination' smart constructor.
data InputDestination = InputDestination'{_idURL ::
                                          !(Maybe Text),
                                          _idIP :: !(Maybe Text),
                                          _idVPC ::
                                          !(Maybe InputDestinationVPC),
                                          _idPort :: !(Maybe Text)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idURL' - This represents the endpoint that the customer stream will be pushed to.
--
-- * 'idIP' - The system-generated static IP address of endpoint. It remains fixed for the lifetime of the input.
--
-- * 'idVPC' - Undocumented member.
--
-- * 'idPort' - The port number for the input.
inputDestination
    :: InputDestination
inputDestination
  = InputDestination'{_idURL = Nothing,
                      _idIP = Nothing, _idVPC = Nothing, _idPort = Nothing}

-- | This represents the endpoint that the customer stream will be pushed to.
idURL :: Lens' InputDestination (Maybe Text)
idURL = lens _idURL (\ s a -> s{_idURL = a})

-- | The system-generated static IP address of endpoint. It remains fixed for the lifetime of the input.
idIP :: Lens' InputDestination (Maybe Text)
idIP = lens _idIP (\ s a -> s{_idIP = a})

-- | Undocumented member.
idVPC :: Lens' InputDestination (Maybe InputDestinationVPC)
idVPC = lens _idVPC (\ s a -> s{_idVPC = a})

-- | The port number for the input.
idPort :: Lens' InputDestination (Maybe Text)
idPort = lens _idPort (\ s a -> s{_idPort = a})

instance FromJSON InputDestination where
        parseJSON
          = withObject "InputDestination"
              (\ x ->
                 InputDestination' <$>
                   (x .:? "url") <*> (x .:? "ip") <*> (x .:? "vpc") <*>
                     (x .:? "port"))

instance Hashable InputDestination where

instance NFData InputDestination where

-- | Endpoint settings for a PUSH type input.
--
-- /See:/ 'inputDestinationRequest' smart constructor.
newtype InputDestinationRequest = InputDestinationRequest'{_idrStreamName
                                                           :: Maybe Text}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'InputDestinationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idrStreamName' - A unique name for the location the RTMP stream is being pushed to.
inputDestinationRequest
    :: InputDestinationRequest
inputDestinationRequest
  = InputDestinationRequest'{_idrStreamName = Nothing}

-- | A unique name for the location the RTMP stream is being pushed to.
idrStreamName :: Lens' InputDestinationRequest (Maybe Text)
idrStreamName = lens _idrStreamName (\ s a -> s{_idrStreamName = a})

instance Hashable InputDestinationRequest where

instance NFData InputDestinationRequest where

instance ToJSON InputDestinationRequest where
        toJSON InputDestinationRequest'{..}
          = object
              (catMaybes [("streamName" .=) <$> _idrStreamName])

-- | The properties for a VPC type input destination.
--
-- /See:/ 'inputDestinationVPC' smart constructor.
data InputDestinationVPC = InputDestinationVPC'{_idvNetworkInterfaceId
                                                :: !(Maybe Text),
                                                _idvAvailabilityZone ::
                                                !(Maybe Text)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDestinationVPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idvNetworkInterfaceId' - The network interface ID of the Input destination in the VPC.
--
-- * 'idvAvailabilityZone' - The availability zone of the Input destination.
inputDestinationVPC
    :: InputDestinationVPC
inputDestinationVPC
  = InputDestinationVPC'{_idvNetworkInterfaceId =
                           Nothing,
                         _idvAvailabilityZone = Nothing}

-- | The network interface ID of the Input destination in the VPC.
idvNetworkInterfaceId :: Lens' InputDestinationVPC (Maybe Text)
idvNetworkInterfaceId = lens _idvNetworkInterfaceId (\ s a -> s{_idvNetworkInterfaceId = a})

-- | The availability zone of the Input destination.
idvAvailabilityZone :: Lens' InputDestinationVPC (Maybe Text)
idvAvailabilityZone = lens _idvAvailabilityZone (\ s a -> s{_idvAvailabilityZone = a})

instance FromJSON InputDestinationVPC where
        parseJSON
          = withObject "InputDestinationVPC"
              (\ x ->
                 InputDestinationVPC' <$>
                   (x .:? "networkInterfaceId") <*>
                     (x .:? "availabilityZone"))

instance Hashable InputDestinationVPC where

instance NFData InputDestinationVPC where

-- | Configurable settings for the input device.
--
-- /See:/ 'inputDeviceConfigurableSettings' smart constructor.
data InputDeviceConfigurableSettings = InputDeviceConfigurableSettings'{_idcsConfiguredInput
                                                                        ::
                                                                        !(Maybe
                                                                            InputDeviceConfiguredInput),
                                                                        _idcsMaxBitrate
                                                                        ::
                                                                        !(Maybe
                                                                            Int)}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'InputDeviceConfigurableSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idcsConfiguredInput' - The input source that you want to use. If the device has a source connected to only one of its input ports, or if you don't care which source the device sends, specify Auto. If the device has sources connected to both its input ports, and you want to use a specific source, specify the source.
--
-- * 'idcsMaxBitrate' - The maximum bitrate in bits per second. Set a value here to throttle the bitrate of the source video.
inputDeviceConfigurableSettings
    :: InputDeviceConfigurableSettings
inputDeviceConfigurableSettings
  = InputDeviceConfigurableSettings'{_idcsConfiguredInput
                                       = Nothing,
                                     _idcsMaxBitrate = Nothing}

-- | The input source that you want to use. If the device has a source connected to only one of its input ports, or if you don't care which source the device sends, specify Auto. If the device has sources connected to both its input ports, and you want to use a specific source, specify the source.
idcsConfiguredInput :: Lens' InputDeviceConfigurableSettings (Maybe InputDeviceConfiguredInput)
idcsConfiguredInput = lens _idcsConfiguredInput (\ s a -> s{_idcsConfiguredInput = a})

-- | The maximum bitrate in bits per second. Set a value here to throttle the bitrate of the source video.
idcsMaxBitrate :: Lens' InputDeviceConfigurableSettings (Maybe Int)
idcsMaxBitrate = lens _idcsMaxBitrate (\ s a -> s{_idcsMaxBitrate = a})

instance Hashable InputDeviceConfigurableSettings
         where

instance NFData InputDeviceConfigurableSettings where

instance ToJSON InputDeviceConfigurableSettings where
        toJSON InputDeviceConfigurableSettings'{..}
          = object
              (catMaybes
                 [("configuredInput" .=) <$> _idcsConfiguredInput,
                  ("maxBitrate" .=) <$> _idcsMaxBitrate])

-- | Settings that describe the active source from the input device, and the video characteristics of that source.
--
-- /See:/ 'inputDeviceHdSettings' smart constructor.
data InputDeviceHdSettings = InputDeviceHdSettings'{_idhsFramerate
                                                    :: !(Maybe Double),
                                                    _idhsScanType ::
                                                    !(Maybe
                                                        InputDeviceScanType),
                                                    _idhsDeviceState ::
                                                    !(Maybe InputDeviceState),
                                                    _idhsHeight :: !(Maybe Int),
                                                    _idhsActiveInput ::
                                                    !(Maybe
                                                        InputDeviceActiveInput),
                                                    _idhsWidth :: !(Maybe Int),
                                                    _idhsConfiguredInput ::
                                                    !(Maybe
                                                        InputDeviceConfiguredInput),
                                                    _idhsMaxBitrate ::
                                                    !(Maybe Int)}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'InputDeviceHdSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idhsFramerate' - The frame rate of the video source.
--
-- * 'idhsScanType' - The scan type of the video source.
--
-- * 'idhsDeviceState' - The state of the input device.
--
-- * 'idhsHeight' - The height of the video source, in pixels.
--
-- * 'idhsActiveInput' - If you specified Auto as the configured input, specifies which of the sources is currently active (SDI or HDMI).
--
-- * 'idhsWidth' - The width of the video source, in pixels.
--
-- * 'idhsConfiguredInput' - The source at the input device that is currently active. You can specify this source.
--
-- * 'idhsMaxBitrate' - The current maximum bitrate for ingesting this source, in bits per second. You can specify this maximum.
inputDeviceHdSettings
    :: InputDeviceHdSettings
inputDeviceHdSettings
  = InputDeviceHdSettings'{_idhsFramerate = Nothing,
                           _idhsScanType = Nothing, _idhsDeviceState = Nothing,
                           _idhsHeight = Nothing, _idhsActiveInput = Nothing,
                           _idhsWidth = Nothing, _idhsConfiguredInput = Nothing,
                           _idhsMaxBitrate = Nothing}

-- | The frame rate of the video source.
idhsFramerate :: Lens' InputDeviceHdSettings (Maybe Double)
idhsFramerate = lens _idhsFramerate (\ s a -> s{_idhsFramerate = a})

-- | The scan type of the video source.
idhsScanType :: Lens' InputDeviceHdSettings (Maybe InputDeviceScanType)
idhsScanType = lens _idhsScanType (\ s a -> s{_idhsScanType = a})

-- | The state of the input device.
idhsDeviceState :: Lens' InputDeviceHdSettings (Maybe InputDeviceState)
idhsDeviceState = lens _idhsDeviceState (\ s a -> s{_idhsDeviceState = a})

-- | The height of the video source, in pixels.
idhsHeight :: Lens' InputDeviceHdSettings (Maybe Int)
idhsHeight = lens _idhsHeight (\ s a -> s{_idhsHeight = a})

-- | If you specified Auto as the configured input, specifies which of the sources is currently active (SDI or HDMI).
idhsActiveInput :: Lens' InputDeviceHdSettings (Maybe InputDeviceActiveInput)
idhsActiveInput = lens _idhsActiveInput (\ s a -> s{_idhsActiveInput = a})

-- | The width of the video source, in pixels.
idhsWidth :: Lens' InputDeviceHdSettings (Maybe Int)
idhsWidth = lens _idhsWidth (\ s a -> s{_idhsWidth = a})

-- | The source at the input device that is currently active. You can specify this source.
idhsConfiguredInput :: Lens' InputDeviceHdSettings (Maybe InputDeviceConfiguredInput)
idhsConfiguredInput = lens _idhsConfiguredInput (\ s a -> s{_idhsConfiguredInput = a})

-- | The current maximum bitrate for ingesting this source, in bits per second. You can specify this maximum.
idhsMaxBitrate :: Lens' InputDeviceHdSettings (Maybe Int)
idhsMaxBitrate = lens _idhsMaxBitrate (\ s a -> s{_idhsMaxBitrate = a})

instance FromJSON InputDeviceHdSettings where
        parseJSON
          = withObject "InputDeviceHdSettings"
              (\ x ->
                 InputDeviceHdSettings' <$>
                   (x .:? "framerate") <*> (x .:? "scanType") <*>
                     (x .:? "deviceState")
                     <*> (x .:? "height")
                     <*> (x .:? "activeInput")
                     <*> (x .:? "width")
                     <*> (x .:? "configuredInput")
                     <*> (x .:? "maxBitrate"))

instance Hashable InputDeviceHdSettings where

instance NFData InputDeviceHdSettings where

-- | The network settings for the input device.
--
-- /See:/ 'inputDeviceNetworkSettings' smart constructor.
data InputDeviceNetworkSettings = InputDeviceNetworkSettings'{_idnsIPAddress
                                                              :: !(Maybe Text),
                                                              _idnsGateway ::
                                                              !(Maybe Text),
                                                              _idnsDNSAddresses
                                                              ::
                                                              !(Maybe [Text]),
                                                              _idnsIPScheme ::
                                                              !(Maybe
                                                                  InputDeviceIPScheme),
                                                              _idnsSubnetMask ::
                                                              !(Maybe Text)}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'InputDeviceNetworkSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idnsIPAddress' - The IP address of the input device.
--
-- * 'idnsGateway' - The network gateway IP address.
--
-- * 'idnsDNSAddresses' - The DNS addresses of the input device.
--
-- * 'idnsIPScheme' - Specifies whether the input device has been configured (outside of MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP address.
--
-- * 'idnsSubnetMask' - The subnet mask of the input device.
inputDeviceNetworkSettings
    :: InputDeviceNetworkSettings
inputDeviceNetworkSettings
  = InputDeviceNetworkSettings'{_idnsIPAddress =
                                  Nothing,
                                _idnsGateway = Nothing,
                                _idnsDNSAddresses = Nothing,
                                _idnsIPScheme = Nothing,
                                _idnsSubnetMask = Nothing}

-- | The IP address of the input device.
idnsIPAddress :: Lens' InputDeviceNetworkSettings (Maybe Text)
idnsIPAddress = lens _idnsIPAddress (\ s a -> s{_idnsIPAddress = a})

-- | The network gateway IP address.
idnsGateway :: Lens' InputDeviceNetworkSettings (Maybe Text)
idnsGateway = lens _idnsGateway (\ s a -> s{_idnsGateway = a})

-- | The DNS addresses of the input device.
idnsDNSAddresses :: Lens' InputDeviceNetworkSettings [Text]
idnsDNSAddresses = lens _idnsDNSAddresses (\ s a -> s{_idnsDNSAddresses = a}) . _Default . _Coerce

-- | Specifies whether the input device has been configured (outside of MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP address.
idnsIPScheme :: Lens' InputDeviceNetworkSettings (Maybe InputDeviceIPScheme)
idnsIPScheme = lens _idnsIPScheme (\ s a -> s{_idnsIPScheme = a})

-- | The subnet mask of the input device.
idnsSubnetMask :: Lens' InputDeviceNetworkSettings (Maybe Text)
idnsSubnetMask = lens _idnsSubnetMask (\ s a -> s{_idnsSubnetMask = a})

instance FromJSON InputDeviceNetworkSettings where
        parseJSON
          = withObject "InputDeviceNetworkSettings"
              (\ x ->
                 InputDeviceNetworkSettings' <$>
                   (x .:? "ipAddress") <*> (x .:? "gateway") <*>
                     (x .:? "dnsAddresses" .!= mempty)
                     <*> (x .:? "ipScheme")
                     <*> (x .:? "subnetMask"))

instance Hashable InputDeviceNetworkSettings where

instance NFData InputDeviceNetworkSettings where

-- | Settings for an input device.
--
-- /See:/ 'inputDeviceRequest' smart constructor.
newtype InputDeviceRequest = InputDeviceRequest'{_idrId
                                                 :: Maybe Text}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'InputDeviceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idrId' - The unique ID for the device.
inputDeviceRequest
    :: InputDeviceRequest
inputDeviceRequest
  = InputDeviceRequest'{_idrId = Nothing}

-- | The unique ID for the device.
idrId :: Lens' InputDeviceRequest (Maybe Text)
idrId = lens _idrId (\ s a -> s{_idrId = a})

instance Hashable InputDeviceRequest where

instance NFData InputDeviceRequest where

instance ToJSON InputDeviceRequest where
        toJSON InputDeviceRequest'{..}
          = object (catMaybes [("id" .=) <$> _idrId])

-- | Settings for an input device.
--
-- /See:/ 'inputDeviceSettings' smart constructor.
newtype InputDeviceSettings = InputDeviceSettings'{_idssId
                                                   :: Maybe Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'InputDeviceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idssId' - The unique ID for the device.
inputDeviceSettings
    :: InputDeviceSettings
inputDeviceSettings
  = InputDeviceSettings'{_idssId = Nothing}

-- | The unique ID for the device.
idssId :: Lens' InputDeviceSettings (Maybe Text)
idssId = lens _idssId (\ s a -> s{_idssId = a})

instance FromJSON InputDeviceSettings where
        parseJSON
          = withObject "InputDeviceSettings"
              (\ x -> InputDeviceSettings' <$> (x .:? "id"))

instance Hashable InputDeviceSettings where

instance NFData InputDeviceSettings where

instance ToJSON InputDeviceSettings where
        toJSON InputDeviceSettings'{..}
          = object (catMaybes [("id" .=) <$> _idssId])

-- | Details of the input device.
--
-- /See:/ 'inputDeviceSummary' smart constructor.
data InputDeviceSummary = InputDeviceSummary'{_idsARN
                                              :: !(Maybe Text),
                                              _idsMACAddress :: !(Maybe Text),
                                              _idsHdDeviceSettings ::
                                              !(Maybe InputDeviceHdSettings),
                                              _idsName :: !(Maybe Text),
                                              _idsId :: !(Maybe Text),
                                              _idsDeviceSettingsSyncState ::
                                              !(Maybe DeviceSettingsSyncState),
                                              _idsType ::
                                              !(Maybe InputDeviceType),
                                              _idsSerialNumber :: !(Maybe Text),
                                              _idsNetworkSettings ::
                                              !(Maybe
                                                  InputDeviceNetworkSettings),
                                              _idsConnectionState ::
                                              !(Maybe
                                                  InputDeviceConnectionState)}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDeviceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idsARN' - The unique ARN of the input device.
--
-- * 'idsMACAddress' - The network MAC address of the input device.
--
-- * 'idsHdDeviceSettings' - Settings that describe an input device that is type HD.
--
-- * 'idsName' - A name that you specify for the input device.
--
-- * 'idsId' - The unique ID of the input device.
--
-- * 'idsDeviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- * 'idsType' - The type of the input device.
--
-- * 'idsSerialNumber' - The unique serial number of the input device.
--
-- * 'idsNetworkSettings' - Network settings for the input device.
--
-- * 'idsConnectionState' - The state of the connection between the input device and AWS.
inputDeviceSummary
    :: InputDeviceSummary
inputDeviceSummary
  = InputDeviceSummary'{_idsARN = Nothing,
                        _idsMACAddress = Nothing,
                        _idsHdDeviceSettings = Nothing, _idsName = Nothing,
                        _idsId = Nothing,
                        _idsDeviceSettingsSyncState = Nothing,
                        _idsType = Nothing, _idsSerialNumber = Nothing,
                        _idsNetworkSettings = Nothing,
                        _idsConnectionState = Nothing}

-- | The unique ARN of the input device.
idsARN :: Lens' InputDeviceSummary (Maybe Text)
idsARN = lens _idsARN (\ s a -> s{_idsARN = a})

-- | The network MAC address of the input device.
idsMACAddress :: Lens' InputDeviceSummary (Maybe Text)
idsMACAddress = lens _idsMACAddress (\ s a -> s{_idsMACAddress = a})

-- | Settings that describe an input device that is type HD.
idsHdDeviceSettings :: Lens' InputDeviceSummary (Maybe InputDeviceHdSettings)
idsHdDeviceSettings = lens _idsHdDeviceSettings (\ s a -> s{_idsHdDeviceSettings = a})

-- | A name that you specify for the input device.
idsName :: Lens' InputDeviceSummary (Maybe Text)
idsName = lens _idsName (\ s a -> s{_idsName = a})

-- | The unique ID of the input device.
idsId :: Lens' InputDeviceSummary (Maybe Text)
idsId = lens _idsId (\ s a -> s{_idsId = a})

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
idsDeviceSettingsSyncState :: Lens' InputDeviceSummary (Maybe DeviceSettingsSyncState)
idsDeviceSettingsSyncState = lens _idsDeviceSettingsSyncState (\ s a -> s{_idsDeviceSettingsSyncState = a})

-- | The type of the input device.
idsType :: Lens' InputDeviceSummary (Maybe InputDeviceType)
idsType = lens _idsType (\ s a -> s{_idsType = a})

-- | The unique serial number of the input device.
idsSerialNumber :: Lens' InputDeviceSummary (Maybe Text)
idsSerialNumber = lens _idsSerialNumber (\ s a -> s{_idsSerialNumber = a})

-- | Network settings for the input device.
idsNetworkSettings :: Lens' InputDeviceSummary (Maybe InputDeviceNetworkSettings)
idsNetworkSettings = lens _idsNetworkSettings (\ s a -> s{_idsNetworkSettings = a})

-- | The state of the connection between the input device and AWS.
idsConnectionState :: Lens' InputDeviceSummary (Maybe InputDeviceConnectionState)
idsConnectionState = lens _idsConnectionState (\ s a -> s{_idsConnectionState = a})

instance FromJSON InputDeviceSummary where
        parseJSON
          = withObject "InputDeviceSummary"
              (\ x ->
                 InputDeviceSummary' <$>
                   (x .:? "arn") <*> (x .:? "macAddress") <*>
                     (x .:? "hdDeviceSettings")
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "deviceSettingsSyncState")
                     <*> (x .:? "type")
                     <*> (x .:? "serialNumber")
                     <*> (x .:? "networkSettings")
                     <*> (x .:? "connectionState"))

instance Hashable InputDeviceSummary where

instance NFData InputDeviceSummary where

-- | Input Location
--
-- /See:/ 'inputLocation' smart constructor.
data InputLocation = InputLocation'{_ilUsername ::
                                    !(Maybe Text),
                                    _ilPasswordParam :: !(Maybe Text),
                                    _ilURI :: !Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilUsername' - Documentation update needed
--
-- * 'ilPasswordParam' - key used to extract the password from EC2 Parameter store
--
-- * 'ilURI' - Uniform Resource Identifier - This should be a path to a file accessible to the Live system (eg. a http:// URI) depending on the output type. For example, a RTMP destination should have a uri simliar to: "rtmp://fmsserver/live".
inputLocation
    :: Text -- ^ 'ilURI'
    -> InputLocation
inputLocation pURI_
  = InputLocation'{_ilUsername = Nothing,
                   _ilPasswordParam = Nothing, _ilURI = pURI_}

-- | Documentation update needed
ilUsername :: Lens' InputLocation (Maybe Text)
ilUsername = lens _ilUsername (\ s a -> s{_ilUsername = a})

-- | key used to extract the password from EC2 Parameter store
ilPasswordParam :: Lens' InputLocation (Maybe Text)
ilPasswordParam = lens _ilPasswordParam (\ s a -> s{_ilPasswordParam = a})

-- | Uniform Resource Identifier - This should be a path to a file accessible to the Live system (eg. a http:// URI) depending on the output type. For example, a RTMP destination should have a uri simliar to: "rtmp://fmsserver/live".
ilURI :: Lens' InputLocation Text
ilURI = lens _ilURI (\ s a -> s{_ilURI = a})

instance FromJSON InputLocation where
        parseJSON
          = withObject "InputLocation"
              (\ x ->
                 InputLocation' <$>
                   (x .:? "username") <*> (x .:? "passwordParam") <*>
                     (x .: "uri"))

instance Hashable InputLocation where

instance NFData InputLocation where

instance ToJSON InputLocation where
        toJSON InputLocation'{..}
          = object
              (catMaybes
                 [("username" .=) <$> _ilUsername,
                  ("passwordParam" .=) <$> _ilPasswordParam,
                  Just ("uri" .= _ilURI)])

-- | Input Loss Behavior
--
-- /See:/ 'inputLossBehavior' smart constructor.
data InputLossBehavior = InputLossBehavior'{_ilbInputLossImageColor
                                            :: !(Maybe Text),
                                            _ilbBlackFrameMsec :: !(Maybe Nat),
                                            _ilbRepeatFrameMsec :: !(Maybe Nat),
                                            _ilbInputLossImageType ::
                                            !(Maybe InputLossImageType),
                                            _ilbInputLossImageSlate ::
                                            !(Maybe InputLocation)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputLossBehavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilbInputLossImageColor' - When input loss image type is "color" this field specifies the color to use. Value: 6 hex characters representing the values of RGB.
--
-- * 'ilbBlackFrameMsec' - Documentation update needed
--
-- * 'ilbRepeatFrameMsec' - Documentation update needed
--
-- * 'ilbInputLossImageType' - Indicates whether to substitute a solid color or a slate into the output after input loss exceeds blackFrameMsec.
--
-- * 'ilbInputLossImageSlate' - When input loss image type is "slate" these fields specify the parameters for accessing the slate.
inputLossBehavior
    :: InputLossBehavior
inputLossBehavior
  = InputLossBehavior'{_ilbInputLossImageColor =
                         Nothing,
                       _ilbBlackFrameMsec = Nothing,
                       _ilbRepeatFrameMsec = Nothing,
                       _ilbInputLossImageType = Nothing,
                       _ilbInputLossImageSlate = Nothing}

-- | When input loss image type is "color" this field specifies the color to use. Value: 6 hex characters representing the values of RGB.
ilbInputLossImageColor :: Lens' InputLossBehavior (Maybe Text)
ilbInputLossImageColor = lens _ilbInputLossImageColor (\ s a -> s{_ilbInputLossImageColor = a})

-- | Documentation update needed
ilbBlackFrameMsec :: Lens' InputLossBehavior (Maybe Natural)
ilbBlackFrameMsec = lens _ilbBlackFrameMsec (\ s a -> s{_ilbBlackFrameMsec = a}) . mapping _Nat

-- | Documentation update needed
ilbRepeatFrameMsec :: Lens' InputLossBehavior (Maybe Natural)
ilbRepeatFrameMsec = lens _ilbRepeatFrameMsec (\ s a -> s{_ilbRepeatFrameMsec = a}) . mapping _Nat

-- | Indicates whether to substitute a solid color or a slate into the output after input loss exceeds blackFrameMsec.
ilbInputLossImageType :: Lens' InputLossBehavior (Maybe InputLossImageType)
ilbInputLossImageType = lens _ilbInputLossImageType (\ s a -> s{_ilbInputLossImageType = a})

-- | When input loss image type is "slate" these fields specify the parameters for accessing the slate.
ilbInputLossImageSlate :: Lens' InputLossBehavior (Maybe InputLocation)
ilbInputLossImageSlate = lens _ilbInputLossImageSlate (\ s a -> s{_ilbInputLossImageSlate = a})

instance FromJSON InputLossBehavior where
        parseJSON
          = withObject "InputLossBehavior"
              (\ x ->
                 InputLossBehavior' <$>
                   (x .:? "inputLossImageColor") <*>
                     (x .:? "blackFrameMsec")
                     <*> (x .:? "repeatFrameMsec")
                     <*> (x .:? "inputLossImageType")
                     <*> (x .:? "inputLossImageSlate"))

instance Hashable InputLossBehavior where

instance NFData InputLossBehavior where

instance ToJSON InputLossBehavior where
        toJSON InputLossBehavior'{..}
          = object
              (catMaybes
                 [("inputLossImageColor" .=) <$>
                    _ilbInputLossImageColor,
                  ("blackFrameMsec" .=) <$> _ilbBlackFrameMsec,
                  ("repeatFrameMsec" .=) <$> _ilbRepeatFrameMsec,
                  ("inputLossImageType" .=) <$> _ilbInputLossImageType,
                  ("inputLossImageSlate" .=) <$>
                    _ilbInputLossImageSlate])

-- | Action to prepare an input for a future immediate input switch.
--
-- /See:/ 'inputPrepareScheduleActionSettings' smart constructor.
data InputPrepareScheduleActionSettings = InputPrepareScheduleActionSettings'{_ipsasInputClippingSettings
                                                                              ::
                                                                              !(Maybe
                                                                                  InputClippingSettings),
                                                                              _ipsasURLPath
                                                                              ::
                                                                              !(Maybe
                                                                                  [Text]),
                                                                              _ipsasInputAttachmentNameReference
                                                                              ::
                                                                              !Text}
                                            deriving (Eq, Read, Show, Data,
                                                      Typeable, Generic)

-- | Creates a value of 'InputPrepareScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsasInputClippingSettings' - Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- * 'ipsasURLPath' - The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
--
-- * 'ipsasInputAttachmentNameReference' - The name of the input attachment that should be prepared by this action. If no name is provided, the action will stop the most recent prepare (if any) when activated.
inputPrepareScheduleActionSettings
    :: Text -- ^ 'ipsasInputAttachmentNameReference'
    -> InputPrepareScheduleActionSettings
inputPrepareScheduleActionSettings
  pInputAttachmentNameReference_
  = InputPrepareScheduleActionSettings'{_ipsasInputClippingSettings
                                          = Nothing,
                                        _ipsasURLPath = Nothing,
                                        _ipsasInputAttachmentNameReference =
                                          pInputAttachmentNameReference_}

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
ipsasInputClippingSettings :: Lens' InputPrepareScheduleActionSettings (Maybe InputClippingSettings)
ipsasInputClippingSettings = lens _ipsasInputClippingSettings (\ s a -> s{_ipsasInputClippingSettings = a})

-- | The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
ipsasURLPath :: Lens' InputPrepareScheduleActionSettings [Text]
ipsasURLPath = lens _ipsasURLPath (\ s a -> s{_ipsasURLPath = a}) . _Default . _Coerce

-- | The name of the input attachment that should be prepared by this action. If no name is provided, the action will stop the most recent prepare (if any) when activated.
ipsasInputAttachmentNameReference :: Lens' InputPrepareScheduleActionSettings Text
ipsasInputAttachmentNameReference = lens _ipsasInputAttachmentNameReference (\ s a -> s{_ipsasInputAttachmentNameReference = a})

instance FromJSON InputPrepareScheduleActionSettings
         where
        parseJSON
          = withObject "InputPrepareScheduleActionSettings"
              (\ x ->
                 InputPrepareScheduleActionSettings' <$>
                   (x .:? "inputClippingSettings") <*>
                     (x .:? "urlPath" .!= mempty)
                     <*> (x .: "inputAttachmentNameReference"))

instance Hashable InputPrepareScheduleActionSettings
         where

instance NFData InputPrepareScheduleActionSettings
         where

instance ToJSON InputPrepareScheduleActionSettings
         where
        toJSON InputPrepareScheduleActionSettings'{..}
          = object
              (catMaybes
                 [("inputClippingSettings" .=) <$>
                    _ipsasInputClippingSettings,
                  ("urlPath" .=) <$> _ipsasURLPath,
                  Just
                    ("inputAttachmentNameReference" .=
                       _ipsasInputAttachmentNameReference)])

-- | An Input Security Group
--
-- /See:/ 'inputSecurityGroup' smart constructor.
data InputSecurityGroup = InputSecurityGroup'{_isgState
                                              ::
                                              !(Maybe InputSecurityGroupState),
                                              _isgARN :: !(Maybe Text),
                                              _isgInputs :: !(Maybe [Text]),
                                              _isgId :: !(Maybe Text),
                                              _isgWhitelistRules ::
                                              !(Maybe [InputWhitelistRule]),
                                              _isgTags ::
                                              !(Maybe (Map Text Text))}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isgState' - The current state of the Input Security Group.
--
-- * 'isgARN' - Unique ARN of Input Security Group
--
-- * 'isgInputs' - The list of inputs currently using this Input Security Group.
--
-- * 'isgId' - The Id of the Input Security Group
--
-- * 'isgWhitelistRules' - Whitelist rules and their sync status
--
-- * 'isgTags' - A collection of key-value pairs.
inputSecurityGroup
    :: InputSecurityGroup
inputSecurityGroup
  = InputSecurityGroup'{_isgState = Nothing,
                        _isgARN = Nothing, _isgInputs = Nothing,
                        _isgId = Nothing, _isgWhitelistRules = Nothing,
                        _isgTags = Nothing}

-- | The current state of the Input Security Group.
isgState :: Lens' InputSecurityGroup (Maybe InputSecurityGroupState)
isgState = lens _isgState (\ s a -> s{_isgState = a})

-- | Unique ARN of Input Security Group
isgARN :: Lens' InputSecurityGroup (Maybe Text)
isgARN = lens _isgARN (\ s a -> s{_isgARN = a})

-- | The list of inputs currently using this Input Security Group.
isgInputs :: Lens' InputSecurityGroup [Text]
isgInputs = lens _isgInputs (\ s a -> s{_isgInputs = a}) . _Default . _Coerce

-- | The Id of the Input Security Group
isgId :: Lens' InputSecurityGroup (Maybe Text)
isgId = lens _isgId (\ s a -> s{_isgId = a})

-- | Whitelist rules and their sync status
isgWhitelistRules :: Lens' InputSecurityGroup [InputWhitelistRule]
isgWhitelistRules = lens _isgWhitelistRules (\ s a -> s{_isgWhitelistRules = a}) . _Default . _Coerce

-- | A collection of key-value pairs.
isgTags :: Lens' InputSecurityGroup (HashMap Text Text)
isgTags = lens _isgTags (\ s a -> s{_isgTags = a}) . _Default . _Map

instance FromJSON InputSecurityGroup where
        parseJSON
          = withObject "InputSecurityGroup"
              (\ x ->
                 InputSecurityGroup' <$>
                   (x .:? "state") <*> (x .:? "arn") <*>
                     (x .:? "inputs" .!= mempty)
                     <*> (x .:? "id")
                     <*> (x .:? "whitelistRules" .!= mempty)
                     <*> (x .:? "tags" .!= mempty))

instance Hashable InputSecurityGroup where

instance NFData InputSecurityGroup where

-- | Live Event input parameters. There can be multiple inputs in a single Live Event.
--
-- /See:/ 'inputSettings' smart constructor.
data InputSettings = InputSettings'{_isVideoSelector
                                    :: !(Maybe VideoSelector),
                                    _isSmpte2038DataPreference ::
                                    !(Maybe Smpte2038DataPreference),
                                    _isNetworkInputSettings ::
                                    !(Maybe NetworkInputSettings),
                                    _isAudioSelectors ::
                                    !(Maybe [AudioSelector]),
                                    _isDeblockFilter ::
                                    !(Maybe InputDeblockFilter),
                                    _isDenoiseFilter ::
                                    !(Maybe InputDenoiseFilter),
                                    _isFilterStrength :: !(Maybe Nat),
                                    _isCaptionSelectors ::
                                    !(Maybe [CaptionSelector]),
                                    _isInputFilter :: !(Maybe InputFilter),
                                    _isSourceEndBehavior ::
                                    !(Maybe InputSourceEndBehavior)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isVideoSelector' - Informs which video elementary stream to decode for input types that have multiple available.
--
-- * 'isSmpte2038DataPreference' - Specifies whether to extract applicable ancillary data from a SMPTE-2038 source in this input. Applicable data types are captions, timecode, AFD, and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in this input, otherwise extract from another source (if any). - IGNORE: Never extract any ancillary data from SMPTE-2038.
--
-- * 'isNetworkInputSettings' - Input settings.
--
-- * 'isAudioSelectors' - Used to select the audio stream to decode for inputs that have multiple available.
--
-- * 'isDeblockFilter' - Enable or disable the deblock filter when filtering.
--
-- * 'isDenoiseFilter' - Enable or disable the denoise filter when filtering.
--
-- * 'isFilterStrength' - Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
--
-- * 'isCaptionSelectors' - Used to select the caption input to use for inputs that have multiple available.
--
-- * 'isInputFilter' - Turns on the filter for this input. MPEG-2 inputs have the deblocking filter enabled by default. 1) auto - filtering will be applied depending on input type/quality 2) disabled - no filtering will be applied to the input 3) forced - filtering will be applied regardless of input type
--
-- * 'isSourceEndBehavior' - Loop input if it is a file. This allows a file input to be streamed indefinitely.
inputSettings
    :: InputSettings
inputSettings
  = InputSettings'{_isVideoSelector = Nothing,
                   _isSmpte2038DataPreference = Nothing,
                   _isNetworkInputSettings = Nothing,
                   _isAudioSelectors = Nothing,
                   _isDeblockFilter = Nothing,
                   _isDenoiseFilter = Nothing,
                   _isFilterStrength = Nothing,
                   _isCaptionSelectors = Nothing,
                   _isInputFilter = Nothing,
                   _isSourceEndBehavior = Nothing}

-- | Informs which video elementary stream to decode for input types that have multiple available.
isVideoSelector :: Lens' InputSettings (Maybe VideoSelector)
isVideoSelector = lens _isVideoSelector (\ s a -> s{_isVideoSelector = a})

-- | Specifies whether to extract applicable ancillary data from a SMPTE-2038 source in this input. Applicable data types are captions, timecode, AFD, and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in this input, otherwise extract from another source (if any). - IGNORE: Never extract any ancillary data from SMPTE-2038.
isSmpte2038DataPreference :: Lens' InputSettings (Maybe Smpte2038DataPreference)
isSmpte2038DataPreference = lens _isSmpte2038DataPreference (\ s a -> s{_isSmpte2038DataPreference = a})

-- | Input settings.
isNetworkInputSettings :: Lens' InputSettings (Maybe NetworkInputSettings)
isNetworkInputSettings = lens _isNetworkInputSettings (\ s a -> s{_isNetworkInputSettings = a})

-- | Used to select the audio stream to decode for inputs that have multiple available.
isAudioSelectors :: Lens' InputSettings [AudioSelector]
isAudioSelectors = lens _isAudioSelectors (\ s a -> s{_isAudioSelectors = a}) . _Default . _Coerce

-- | Enable or disable the deblock filter when filtering.
isDeblockFilter :: Lens' InputSettings (Maybe InputDeblockFilter)
isDeblockFilter = lens _isDeblockFilter (\ s a -> s{_isDeblockFilter = a})

-- | Enable or disable the denoise filter when filtering.
isDenoiseFilter :: Lens' InputSettings (Maybe InputDenoiseFilter)
isDenoiseFilter = lens _isDenoiseFilter (\ s a -> s{_isDenoiseFilter = a})

-- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
isFilterStrength :: Lens' InputSettings (Maybe Natural)
isFilterStrength = lens _isFilterStrength (\ s a -> s{_isFilterStrength = a}) . mapping _Nat

-- | Used to select the caption input to use for inputs that have multiple available.
isCaptionSelectors :: Lens' InputSettings [CaptionSelector]
isCaptionSelectors = lens _isCaptionSelectors (\ s a -> s{_isCaptionSelectors = a}) . _Default . _Coerce

-- | Turns on the filter for this input. MPEG-2 inputs have the deblocking filter enabled by default. 1) auto - filtering will be applied depending on input type/quality 2) disabled - no filtering will be applied to the input 3) forced - filtering will be applied regardless of input type
isInputFilter :: Lens' InputSettings (Maybe InputFilter)
isInputFilter = lens _isInputFilter (\ s a -> s{_isInputFilter = a})

-- | Loop input if it is a file. This allows a file input to be streamed indefinitely.
isSourceEndBehavior :: Lens' InputSettings (Maybe InputSourceEndBehavior)
isSourceEndBehavior = lens _isSourceEndBehavior (\ s a -> s{_isSourceEndBehavior = a})

instance FromJSON InputSettings where
        parseJSON
          = withObject "InputSettings"
              (\ x ->
                 InputSettings' <$>
                   (x .:? "videoSelector") <*>
                     (x .:? "smpte2038DataPreference")
                     <*> (x .:? "networkInputSettings")
                     <*> (x .:? "audioSelectors" .!= mempty)
                     <*> (x .:? "deblockFilter")
                     <*> (x .:? "denoiseFilter")
                     <*> (x .:? "filterStrength")
                     <*> (x .:? "captionSelectors" .!= mempty)
                     <*> (x .:? "inputFilter")
                     <*> (x .:? "sourceEndBehavior"))

instance Hashable InputSettings where

instance NFData InputSettings where

instance ToJSON InputSettings where
        toJSON InputSettings'{..}
          = object
              (catMaybes
                 [("videoSelector" .=) <$> _isVideoSelector,
                  ("smpte2038DataPreference" .=) <$>
                    _isSmpte2038DataPreference,
                  ("networkInputSettings" .=) <$>
                    _isNetworkInputSettings,
                  ("audioSelectors" .=) <$> _isAudioSelectors,
                  ("deblockFilter" .=) <$> _isDeblockFilter,
                  ("denoiseFilter" .=) <$> _isDenoiseFilter,
                  ("filterStrength" .=) <$> _isFilterStrength,
                  ("captionSelectors" .=) <$> _isCaptionSelectors,
                  ("inputFilter" .=) <$> _isInputFilter,
                  ("sourceEndBehavior" .=) <$> _isSourceEndBehavior])

-- | The settings for a PULL type input.
--
-- /See:/ 'inputSource' smart constructor.
data InputSource = InputSource'{_isURL ::
                                !(Maybe Text),
                                _isUsername :: !(Maybe Text),
                                _isPasswordParam :: !(Maybe Text)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isURL' - This represents the customer's source URL where stream is pulled from.
--
-- * 'isUsername' - The username for the input source.
--
-- * 'isPasswordParam' - The key used to extract the password from EC2 Parameter store.
inputSource
    :: InputSource
inputSource
  = InputSource'{_isURL = Nothing,
                 _isUsername = Nothing, _isPasswordParam = Nothing}

-- | This represents the customer's source URL where stream is pulled from.
isURL :: Lens' InputSource (Maybe Text)
isURL = lens _isURL (\ s a -> s{_isURL = a})

-- | The username for the input source.
isUsername :: Lens' InputSource (Maybe Text)
isUsername = lens _isUsername (\ s a -> s{_isUsername = a})

-- | The key used to extract the password from EC2 Parameter store.
isPasswordParam :: Lens' InputSource (Maybe Text)
isPasswordParam = lens _isPasswordParam (\ s a -> s{_isPasswordParam = a})

instance FromJSON InputSource where
        parseJSON
          = withObject "InputSource"
              (\ x ->
                 InputSource' <$>
                   (x .:? "url") <*> (x .:? "username") <*>
                     (x .:? "passwordParam"))

instance Hashable InputSource where

instance NFData InputSource where

-- | Settings for for a PULL type input.
--
-- /See:/ 'inputSourceRequest' smart constructor.
data InputSourceRequest = InputSourceRequest'{_isrURL
                                              :: !(Maybe Text),
                                              _isrUsername :: !(Maybe Text),
                                              _isrPasswordParam ::
                                              !(Maybe Text)}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSourceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isrURL' - This represents the customer's source URL where stream is pulled from.
--
-- * 'isrUsername' - The username for the input source.
--
-- * 'isrPasswordParam' - The key used to extract the password from EC2 Parameter store.
inputSourceRequest
    :: InputSourceRequest
inputSourceRequest
  = InputSourceRequest'{_isrURL = Nothing,
                        _isrUsername = Nothing, _isrPasswordParam = Nothing}

-- | This represents the customer's source URL where stream is pulled from.
isrURL :: Lens' InputSourceRequest (Maybe Text)
isrURL = lens _isrURL (\ s a -> s{_isrURL = a})

-- | The username for the input source.
isrUsername :: Lens' InputSourceRequest (Maybe Text)
isrUsername = lens _isrUsername (\ s a -> s{_isrUsername = a})

-- | The key used to extract the password from EC2 Parameter store.
isrPasswordParam :: Lens' InputSourceRequest (Maybe Text)
isrPasswordParam = lens _isrPasswordParam (\ s a -> s{_isrPasswordParam = a})

instance Hashable InputSourceRequest where

instance NFData InputSourceRequest where

instance ToJSON InputSourceRequest where
        toJSON InputSourceRequest'{..}
          = object
              (catMaybes
                 [("url" .=) <$> _isrURL,
                  ("username" .=) <$> _isrUsername,
                  ("passwordParam" .=) <$> _isrPasswordParam])

-- | Placeholder documentation for InputSpecification
--
-- /See:/ 'inputSpecification' smart constructor.
data InputSpecification = InputSpecification'{_isResolution
                                              :: !(Maybe InputResolution),
                                              _isCodec :: !(Maybe InputCodec),
                                              _isMaximumBitrate ::
                                              !(Maybe InputMaximumBitrate)}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isResolution' - Input resolution, categorized coarsely
--
-- * 'isCodec' - Input codec
--
-- * 'isMaximumBitrate' - Maximum input bitrate, categorized coarsely
inputSpecification
    :: InputSpecification
inputSpecification
  = InputSpecification'{_isResolution = Nothing,
                        _isCodec = Nothing, _isMaximumBitrate = Nothing}

-- | Input resolution, categorized coarsely
isResolution :: Lens' InputSpecification (Maybe InputResolution)
isResolution = lens _isResolution (\ s a -> s{_isResolution = a})

-- | Input codec
isCodec :: Lens' InputSpecification (Maybe InputCodec)
isCodec = lens _isCodec (\ s a -> s{_isCodec = a})

-- | Maximum input bitrate, categorized coarsely
isMaximumBitrate :: Lens' InputSpecification (Maybe InputMaximumBitrate)
isMaximumBitrate = lens _isMaximumBitrate (\ s a -> s{_isMaximumBitrate = a})

instance FromJSON InputSpecification where
        parseJSON
          = withObject "InputSpecification"
              (\ x ->
                 InputSpecification' <$>
                   (x .:? "resolution") <*> (x .:? "codec") <*>
                     (x .:? "maximumBitrate"))

instance Hashable InputSpecification where

instance NFData InputSpecification where

instance ToJSON InputSpecification where
        toJSON InputSpecification'{..}
          = object
              (catMaybes
                 [("resolution" .=) <$> _isResolution,
                  ("codec" .=) <$> _isCodec,
                  ("maximumBitrate" .=) <$> _isMaximumBitrate])

-- | Settings for the "switch input" action: to switch from ingesting one input to ingesting another input.
--
-- /See:/ 'inputSwitchScheduleActionSettings' smart constructor.
data InputSwitchScheduleActionSettings = InputSwitchScheduleActionSettings'{_issasInputClippingSettings
                                                                            ::
                                                                            !(Maybe
                                                                                InputClippingSettings),
                                                                            _issasURLPath
                                                                            ::
                                                                            !(Maybe
                                                                                [Text]),
                                                                            _issasInputAttachmentNameReference
                                                                            ::
                                                                            !Text}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'InputSwitchScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'issasInputClippingSettings' - Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- * 'issasURLPath' - The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
--
-- * 'issasInputAttachmentNameReference' - The name of the input attachment (not the name of the input!) to switch to. The name is specified in the channel configuration.
inputSwitchScheduleActionSettings
    :: Text -- ^ 'issasInputAttachmentNameReference'
    -> InputSwitchScheduleActionSettings
inputSwitchScheduleActionSettings
  pInputAttachmentNameReference_
  = InputSwitchScheduleActionSettings'{_issasInputClippingSettings
                                         = Nothing,
                                       _issasURLPath = Nothing,
                                       _issasInputAttachmentNameReference =
                                         pInputAttachmentNameReference_}

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
issasInputClippingSettings :: Lens' InputSwitchScheduleActionSettings (Maybe InputClippingSettings)
issasInputClippingSettings = lens _issasInputClippingSettings (\ s a -> s{_issasInputClippingSettings = a})

-- | The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
issasURLPath :: Lens' InputSwitchScheduleActionSettings [Text]
issasURLPath = lens _issasURLPath (\ s a -> s{_issasURLPath = a}) . _Default . _Coerce

-- | The name of the input attachment (not the name of the input!) to switch to. The name is specified in the channel configuration.
issasInputAttachmentNameReference :: Lens' InputSwitchScheduleActionSettings Text
issasInputAttachmentNameReference = lens _issasInputAttachmentNameReference (\ s a -> s{_issasInputAttachmentNameReference = a})

instance FromJSON InputSwitchScheduleActionSettings
         where
        parseJSON
          = withObject "InputSwitchScheduleActionSettings"
              (\ x ->
                 InputSwitchScheduleActionSettings' <$>
                   (x .:? "inputClippingSettings") <*>
                     (x .:? "urlPath" .!= mempty)
                     <*> (x .: "inputAttachmentNameReference"))

instance Hashable InputSwitchScheduleActionSettings
         where

instance NFData InputSwitchScheduleActionSettings
         where

instance ToJSON InputSwitchScheduleActionSettings
         where
        toJSON InputSwitchScheduleActionSettings'{..}
          = object
              (catMaybes
                 [("inputClippingSettings" .=) <$>
                    _issasInputClippingSettings,
                  ("urlPath" .=) <$> _issasURLPath,
                  Just
                    ("inputAttachmentNameReference" .=
                       _issasInputAttachmentNameReference)])

-- | Settings for a private VPC Input.
--
-- When this property is specified, the input destination addresses will be created in a VPC rather than with public Internet addresses.
-- This property requires setting the roleArn property on Input creation.
-- Not compatible with the inputSecurityGroups property.
--
-- /See:/ 'inputVPCRequest' smart constructor.
data InputVPCRequest = InputVPCRequest'{_ivrSecurityGroupIds
                                        :: !(Maybe [Text]),
                                        _ivrSubnetIds :: ![Text]}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputVPCRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivrSecurityGroupIds' - A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC network interfaces. Requires subnetIds. If none are specified then the VPC default security group will be used.
--
-- * 'ivrSubnetIds' - A list of 2 VPC subnet IDs from the same VPC. Subnet IDs must be mapped to two unique availability zones (AZ).
inputVPCRequest
    :: InputVPCRequest
inputVPCRequest
  = InputVPCRequest'{_ivrSecurityGroupIds = Nothing,
                     _ivrSubnetIds = mempty}

-- | A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC network interfaces. Requires subnetIds. If none are specified then the VPC default security group will be used.
ivrSecurityGroupIds :: Lens' InputVPCRequest [Text]
ivrSecurityGroupIds = lens _ivrSecurityGroupIds (\ s a -> s{_ivrSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of 2 VPC subnet IDs from the same VPC. Subnet IDs must be mapped to two unique availability zones (AZ).
ivrSubnetIds :: Lens' InputVPCRequest [Text]
ivrSubnetIds = lens _ivrSubnetIds (\ s a -> s{_ivrSubnetIds = a}) . _Coerce

instance Hashable InputVPCRequest where

instance NFData InputVPCRequest where

instance ToJSON InputVPCRequest where
        toJSON InputVPCRequest'{..}
          = object
              (catMaybes
                 [("securityGroupIds" .=) <$> _ivrSecurityGroupIds,
                  Just ("subnetIds" .= _ivrSubnetIds)])

-- | Whitelist rule
--
-- /See:/ 'inputWhitelistRule' smart constructor.
newtype InputWhitelistRule = InputWhitelistRule'{_iwrCidr
                                                 :: Maybe Text}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'InputWhitelistRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iwrCidr' - The IPv4 CIDR that's whitelisted.
inputWhitelistRule
    :: InputWhitelistRule
inputWhitelistRule
  = InputWhitelistRule'{_iwrCidr = Nothing}

-- | The IPv4 CIDR that's whitelisted.
iwrCidr :: Lens' InputWhitelistRule (Maybe Text)
iwrCidr = lens _iwrCidr (\ s a -> s{_iwrCidr = a})

instance FromJSON InputWhitelistRule where
        parseJSON
          = withObject "InputWhitelistRule"
              (\ x -> InputWhitelistRule' <$> (x .:? "cidr"))

instance Hashable InputWhitelistRule where

instance NFData InputWhitelistRule where

-- | An IPv4 CIDR to whitelist.
--
-- /See:/ 'inputWhitelistRuleCidr' smart constructor.
newtype InputWhitelistRuleCidr = InputWhitelistRuleCidr'{_iwrcCidr
                                                         :: Maybe Text}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'InputWhitelistRuleCidr' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iwrcCidr' - The IPv4 CIDR to whitelist.
inputWhitelistRuleCidr
    :: InputWhitelistRuleCidr
inputWhitelistRuleCidr
  = InputWhitelistRuleCidr'{_iwrcCidr = Nothing}

-- | The IPv4 CIDR to whitelist.
iwrcCidr :: Lens' InputWhitelistRuleCidr (Maybe Text)
iwrcCidr = lens _iwrcCidr (\ s a -> s{_iwrcCidr = a})

instance Hashable InputWhitelistRuleCidr where

instance NFData InputWhitelistRuleCidr where

instance ToJSON InputWhitelistRuleCidr where
        toJSON InputWhitelistRuleCidr'{..}
          = object (catMaybes [("cidr" .=) <$> _iwrcCidr])

-- | Key Provider Settings
--
-- /See:/ 'keyProviderSettings' smart constructor.
newtype KeyProviderSettings = KeyProviderSettings'{_kpsStaticKeySettings
                                                   :: Maybe StaticKeySettings}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'KeyProviderSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpsStaticKeySettings' - Undocumented member.
keyProviderSettings
    :: KeyProviderSettings
keyProviderSettings
  = KeyProviderSettings'{_kpsStaticKeySettings =
                           Nothing}

-- | Undocumented member.
kpsStaticKeySettings :: Lens' KeyProviderSettings (Maybe StaticKeySettings)
kpsStaticKeySettings = lens _kpsStaticKeySettings (\ s a -> s{_kpsStaticKeySettings = a})

instance FromJSON KeyProviderSettings where
        parseJSON
          = withObject "KeyProviderSettings"
              (\ x ->
                 KeyProviderSettings' <$> (x .:? "staticKeySettings"))

instance Hashable KeyProviderSettings where

instance NFData KeyProviderSettings where

instance ToJSON KeyProviderSettings where
        toJSON KeyProviderSettings'{..}
          = object
              (catMaybes
                 [("staticKeySettings" .=) <$> _kpsStaticKeySettings])

-- | M2ts Settings
--
-- /See:/ 'm2tsSettings' smart constructor.
data M2tsSettings = M2tsSettings'{_msPmtPid ::
                                  !(Maybe Text),
                                  _msEtvSignalPid :: !(Maybe Text),
                                  _msVideoPid :: !(Maybe Text),
                                  _msNielsenId3Behavior ::
                                  !(Maybe M2tsNielsenId3Behavior),
                                  _msBufferModel :: !(Maybe M2tsBufferModel),
                                  _msScte35Pid :: !(Maybe Text),
                                  _msTransportStreamId :: !(Maybe Nat),
                                  _msProgramNum :: !(Maybe Nat),
                                  _msFragmentTime :: !(Maybe Double),
                                  _msTimedMetadataBehavior ::
                                  !(Maybe M2tsTimedMetadataBehavior),
                                  _msCCDescriptor :: !(Maybe M2tsCCDescriptor),
                                  _msPmtInterval :: !(Maybe Nat),
                                  _msDvbSdtSettings :: !(Maybe DvbSdtSettings),
                                  _msEcmPid :: !(Maybe Text),
                                  _msNullPacketBitrate :: !(Maybe Double),
                                  _msAudioBufferModel ::
                                  !(Maybe M2tsAudioBufferModel),
                                  _msTimedMetadataPid :: !(Maybe Text),
                                  _msKlv :: !(Maybe M2tsKlv),
                                  _msAudioFramesPerPes :: !(Maybe Nat),
                                  _msPcrPeriod :: !(Maybe Nat),
                                  _msPcrPid :: !(Maybe Text),
                                  _msSegmentationMarkers ::
                                  !(Maybe M2tsSegmentationMarkers),
                                  _msAribCaptionsPidControl ::
                                  !(Maybe M2tsAribCaptionsPidControl),
                                  _msKlvDataPids :: !(Maybe Text),
                                  _msEbpLookaheadMs :: !(Maybe Nat),
                                  _msDvbSubPids :: !(Maybe Text),
                                  _msScte27Pids :: !(Maybe Text),
                                  _msPatInterval :: !(Maybe Nat),
                                  _msAudioStreamType ::
                                  !(Maybe M2tsAudioStreamType),
                                  _msEsRateInPes :: !(Maybe M2tsEsRateInPes),
                                  _msEtvPlatformPid :: !(Maybe Text),
                                  _msBitrate :: !(Maybe Nat),
                                  _msScte35Control ::
                                  !(Maybe M2tsScte35Control),
                                  _msAudioPids :: !(Maybe Text),
                                  _msDvbTeletextPid :: !(Maybe Text),
                                  _msEbif :: !(Maybe M2tsEbifControl),
                                  _msArib :: !(Maybe M2tsArib),
                                  _msAribCaptionsPid :: !(Maybe Text),
                                  _msAbsentInputAudioBehavior ::
                                  !(Maybe M2tsAbsentInputAudioBehavior),
                                  _msSegmentationTime :: !(Maybe Double),
                                  _msEbpAudioInterval ::
                                  !(Maybe M2tsAudioInterval),
                                  _msDvbNitSettings :: !(Maybe DvbNitSettings),
                                  _msPcrControl :: !(Maybe M2tsPcrControl),
                                  _msEbpPlacement :: !(Maybe M2tsEbpPlacement),
                                  _msRateMode :: !(Maybe M2tsRateMode),
                                  _msSegmentationStyle ::
                                  !(Maybe M2tsSegmentationStyle),
                                  _msDvbTdtSettings :: !(Maybe DvbTdtSettings)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'M2tsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msPmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value. Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msEtvSignalPid' - Packet Identifier (PID) for input source ETV Signal data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msVideoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msNielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- * 'msBufferModel' - If set to multiplex, use multiplex buffer model for accurate interleaving.  Setting to bufferModel to none can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
--
-- * 'msScte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msTransportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- * 'msProgramNum' - The value of the program number field in the Program Map Table.
--
-- * 'msFragmentTime' - The length in seconds of each fragment. Only used with EBP markers.
--
-- * 'msTimedMetadataBehavior' - When set to passthrough, timed metadata will be passed through from input to output.
--
-- * 'msCCDescriptor' - When set to enabled, generates captionServiceDescriptor in PMT.
--
-- * 'msPmtInterval' - The number of milliseconds between instances of this table in the output transport stream. Valid values are 0, 10..1000.
--
-- * 'msDvbSdtSettings' - Inserts DVB Service Description Table (SDT) at the specified table repetition interval.
--
-- * 'msEcmPid' - This field is unused and deprecated.
--
-- * 'msNullPacketBitrate' - Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
--
-- * 'msAudioBufferModel' - When set to dvb, uses DVB buffer model for Dolby Digital audio.  When set to atsc, the ATSC model is used.
--
-- * 'msTimedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msKlv' - If set to passthrough, passes any KLV data from the input source to this output.
--
-- * 'msAudioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- * 'msPcrPeriod' - Maximum time in milliseconds between Program Clock Reference (PCRs) inserted into the transport stream.
--
-- * 'msPcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msSegmentationMarkers' - Inserts segmentation markers at each segmentationTime period. raiSegstart sets the Random Access Indicator bit in the adaptation field. raiAdapt sets the RAI bit and adds the current timecode in the private data bytes. psiSegstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebpLegacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
--
-- * 'msAribCaptionsPidControl' - If set to auto, pid number used for ARIB Captions will be auto-selected from unused pids.  If set to useConfigured, ARIB Captions will be on the configured pid number.
--
-- * 'msKlvDataPids' - Packet Identifier (PID) for input source KLV data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msEbpLookaheadMs' - When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker.  The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
--
-- * 'msDvbSubPids' - Packet Identifier (PID) for input source DVB Subtitle data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msScte27Pids' - Packet Identifier (PID) for input source SCTE-27 data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msPatInterval' - The number of milliseconds between instances of this table in the output transport stream.  Valid values are 0, 10..1000.
--
-- * 'msAudioStreamType' - When set to atsc, uses stream type = 0x81 for AC3 and stream type = 0x87 for EAC3. When set to dvb, uses stream type = 0x06.
--
-- * 'msEsRateInPes' - Include or exclude the ES Rate field in the PES header.
--
-- * 'msEtvPlatformPid' - Packet Identifier (PID) for input source ETV Platform data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msBitrate' - The output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate.
--
-- * 'msScte35Control' - Optionally pass SCTE-35 signals from the input source to this output.
--
-- * 'msAudioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values. Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msDvbTeletextPid' - Packet Identifier (PID) for input source DVB Teletext data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msEbif' - If set to passthrough, passes any EBIF data from the input source to this output.
--
-- * 'msArib' - When set to enabled, uses ARIB-compliant field muxing and removes video descriptor.
--
-- * 'msAribCaptionsPid' - Packet Identifier (PID) for ARIB Captions in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'msAbsentInputAudioBehavior' - When set to drop, output audio streams will be removed from the program if the selected input audio stream is removed from the input. This allows the output audio configuration to dynamically change based on input configuration. If this is set to encodeSilence, all output audio streams will output encoded silence when not connected to an active input stream.
--
-- * 'msSegmentationTime' - The length in seconds of each segment. Required unless markers is set to _none_.
--
-- * 'msEbpAudioInterval' - When videoAndFixedIntervals is selected, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. Only available when EBP Cablelabs segmentation markers are selected.  Partitions 1 and 2 will always follow the video interval.
--
-- * 'msDvbNitSettings' - Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
--
-- * 'msPcrControl' - When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
--
-- * 'msEbpPlacement' - Controls placement of EBP on Audio PIDs. If set to videoAndAudioPids, EBP markers will be placed on the video PID and all audio PIDs.  If set to videoPid, EBP markers will be placed on only the video PID.
--
-- * 'msRateMode' - When vbr, does not insert null packets into transport stream to fill specified bitrate. The bitrate setting acts as the maximum bitrate when vbr is set.
--
-- * 'msSegmentationStyle' - The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "resetCadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of $segmentationTime seconds. When a segmentation style of "maintainCadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentationTime seconds. Note that EBP lookahead is a slight exception to this rule.
--
-- * 'msDvbTdtSettings' - Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
m2tsSettings
    :: M2tsSettings
m2tsSettings
  = M2tsSettings'{_msPmtPid = Nothing,
                  _msEtvSignalPid = Nothing, _msVideoPid = Nothing,
                  _msNielsenId3Behavior = Nothing,
                  _msBufferModel = Nothing, _msScte35Pid = Nothing,
                  _msTransportStreamId = Nothing,
                  _msProgramNum = Nothing, _msFragmentTime = Nothing,
                  _msTimedMetadataBehavior = Nothing,
                  _msCCDescriptor = Nothing, _msPmtInterval = Nothing,
                  _msDvbSdtSettings = Nothing, _msEcmPid = Nothing,
                  _msNullPacketBitrate = Nothing,
                  _msAudioBufferModel = Nothing,
                  _msTimedMetadataPid = Nothing, _msKlv = Nothing,
                  _msAudioFramesPerPes = Nothing,
                  _msPcrPeriod = Nothing, _msPcrPid = Nothing,
                  _msSegmentationMarkers = Nothing,
                  _msAribCaptionsPidControl = Nothing,
                  _msKlvDataPids = Nothing,
                  _msEbpLookaheadMs = Nothing, _msDvbSubPids = Nothing,
                  _msScte27Pids = Nothing, _msPatInterval = Nothing,
                  _msAudioStreamType = Nothing,
                  _msEsRateInPes = Nothing,
                  _msEtvPlatformPid = Nothing, _msBitrate = Nothing,
                  _msScte35Control = Nothing, _msAudioPids = Nothing,
                  _msDvbTeletextPid = Nothing, _msEbif = Nothing,
                  _msArib = Nothing, _msAribCaptionsPid = Nothing,
                  _msAbsentInputAudioBehavior = Nothing,
                  _msSegmentationTime = Nothing,
                  _msEbpAudioInterval = Nothing,
                  _msDvbNitSettings = Nothing, _msPcrControl = Nothing,
                  _msEbpPlacement = Nothing, _msRateMode = Nothing,
                  _msSegmentationStyle = Nothing,
                  _msDvbTdtSettings = Nothing}

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value. Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msPmtPid :: Lens' M2tsSettings (Maybe Text)
msPmtPid = lens _msPmtPid (\ s a -> s{_msPmtPid = a})

-- | Packet Identifier (PID) for input source ETV Signal data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msEtvSignalPid :: Lens' M2tsSettings (Maybe Text)
msEtvSignalPid = lens _msEtvSignalPid (\ s a -> s{_msEtvSignalPid = a})

-- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msVideoPid :: Lens' M2tsSettings (Maybe Text)
msVideoPid = lens _msVideoPid (\ s a -> s{_msVideoPid = a})

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
msNielsenId3Behavior :: Lens' M2tsSettings (Maybe M2tsNielsenId3Behavior)
msNielsenId3Behavior = lens _msNielsenId3Behavior (\ s a -> s{_msNielsenId3Behavior = a})

-- | If set to multiplex, use multiplex buffer model for accurate interleaving.  Setting to bufferModel to none can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
msBufferModel :: Lens' M2tsSettings (Maybe M2tsBufferModel)
msBufferModel = lens _msBufferModel (\ s a -> s{_msBufferModel = a})

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msScte35Pid :: Lens' M2tsSettings (Maybe Text)
msScte35Pid = lens _msScte35Pid (\ s a -> s{_msScte35Pid = a})

-- | The value of the transport stream ID field in the Program Map Table.
msTransportStreamId :: Lens' M2tsSettings (Maybe Natural)
msTransportStreamId = lens _msTransportStreamId (\ s a -> s{_msTransportStreamId = a}) . mapping _Nat

-- | The value of the program number field in the Program Map Table.
msProgramNum :: Lens' M2tsSettings (Maybe Natural)
msProgramNum = lens _msProgramNum (\ s a -> s{_msProgramNum = a}) . mapping _Nat

-- | The length in seconds of each fragment. Only used with EBP markers.
msFragmentTime :: Lens' M2tsSettings (Maybe Double)
msFragmentTime = lens _msFragmentTime (\ s a -> s{_msFragmentTime = a})

-- | When set to passthrough, timed metadata will be passed through from input to output.
msTimedMetadataBehavior :: Lens' M2tsSettings (Maybe M2tsTimedMetadataBehavior)
msTimedMetadataBehavior = lens _msTimedMetadataBehavior (\ s a -> s{_msTimedMetadataBehavior = a})

-- | When set to enabled, generates captionServiceDescriptor in PMT.
msCCDescriptor :: Lens' M2tsSettings (Maybe M2tsCCDescriptor)
msCCDescriptor = lens _msCCDescriptor (\ s a -> s{_msCCDescriptor = a})

-- | The number of milliseconds between instances of this table in the output transport stream. Valid values are 0, 10..1000.
msPmtInterval :: Lens' M2tsSettings (Maybe Natural)
msPmtInterval = lens _msPmtInterval (\ s a -> s{_msPmtInterval = a}) . mapping _Nat

-- | Inserts DVB Service Description Table (SDT) at the specified table repetition interval.
msDvbSdtSettings :: Lens' M2tsSettings (Maybe DvbSdtSettings)
msDvbSdtSettings = lens _msDvbSdtSettings (\ s a -> s{_msDvbSdtSettings = a})

-- | This field is unused and deprecated.
msEcmPid :: Lens' M2tsSettings (Maybe Text)
msEcmPid = lens _msEcmPid (\ s a -> s{_msEcmPid = a})

-- | Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
msNullPacketBitrate :: Lens' M2tsSettings (Maybe Double)
msNullPacketBitrate = lens _msNullPacketBitrate (\ s a -> s{_msNullPacketBitrate = a})

-- | When set to dvb, uses DVB buffer model for Dolby Digital audio.  When set to atsc, the ATSC model is used.
msAudioBufferModel :: Lens' M2tsSettings (Maybe M2tsAudioBufferModel)
msAudioBufferModel = lens _msAudioBufferModel (\ s a -> s{_msAudioBufferModel = a})

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msTimedMetadataPid :: Lens' M2tsSettings (Maybe Text)
msTimedMetadataPid = lens _msTimedMetadataPid (\ s a -> s{_msTimedMetadataPid = a})

-- | If set to passthrough, passes any KLV data from the input source to this output.
msKlv :: Lens' M2tsSettings (Maybe M2tsKlv)
msKlv = lens _msKlv (\ s a -> s{_msKlv = a})

-- | The number of audio frames to insert for each PES packet.
msAudioFramesPerPes :: Lens' M2tsSettings (Maybe Natural)
msAudioFramesPerPes = lens _msAudioFramesPerPes (\ s a -> s{_msAudioFramesPerPes = a}) . mapping _Nat

-- | Maximum time in milliseconds between Program Clock Reference (PCRs) inserted into the transport stream.
msPcrPeriod :: Lens' M2tsSettings (Maybe Natural)
msPcrPeriod = lens _msPcrPeriod (\ s a -> s{_msPcrPeriod = a}) . mapping _Nat

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msPcrPid :: Lens' M2tsSettings (Maybe Text)
msPcrPid = lens _msPcrPid (\ s a -> s{_msPcrPid = a})

-- | Inserts segmentation markers at each segmentationTime period. raiSegstart sets the Random Access Indicator bit in the adaptation field. raiAdapt sets the RAI bit and adds the current timecode in the private data bytes. psiSegstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebpLegacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
msSegmentationMarkers :: Lens' M2tsSettings (Maybe M2tsSegmentationMarkers)
msSegmentationMarkers = lens _msSegmentationMarkers (\ s a -> s{_msSegmentationMarkers = a})

-- | If set to auto, pid number used for ARIB Captions will be auto-selected from unused pids.  If set to useConfigured, ARIB Captions will be on the configured pid number.
msAribCaptionsPidControl :: Lens' M2tsSettings (Maybe M2tsAribCaptionsPidControl)
msAribCaptionsPidControl = lens _msAribCaptionsPidControl (\ s a -> s{_msAribCaptionsPidControl = a})

-- | Packet Identifier (PID) for input source KLV data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
msKlvDataPids :: Lens' M2tsSettings (Maybe Text)
msKlvDataPids = lens _msKlvDataPids (\ s a -> s{_msKlvDataPids = a})

-- | When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker.  The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
msEbpLookaheadMs :: Lens' M2tsSettings (Maybe Natural)
msEbpLookaheadMs = lens _msEbpLookaheadMs (\ s a -> s{_msEbpLookaheadMs = a}) . mapping _Nat

-- | Packet Identifier (PID) for input source DVB Subtitle data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
msDvbSubPids :: Lens' M2tsSettings (Maybe Text)
msDvbSubPids = lens _msDvbSubPids (\ s a -> s{_msDvbSubPids = a})

-- | Packet Identifier (PID) for input source SCTE-27 data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.  Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
msScte27Pids :: Lens' M2tsSettings (Maybe Text)
msScte27Pids = lens _msScte27Pids (\ s a -> s{_msScte27Pids = a})

-- | The number of milliseconds between instances of this table in the output transport stream.  Valid values are 0, 10..1000.
msPatInterval :: Lens' M2tsSettings (Maybe Natural)
msPatInterval = lens _msPatInterval (\ s a -> s{_msPatInterval = a}) . mapping _Nat

-- | When set to atsc, uses stream type = 0x81 for AC3 and stream type = 0x87 for EAC3. When set to dvb, uses stream type = 0x06.
msAudioStreamType :: Lens' M2tsSettings (Maybe M2tsAudioStreamType)
msAudioStreamType = lens _msAudioStreamType (\ s a -> s{_msAudioStreamType = a})

-- | Include or exclude the ES Rate field in the PES header.
msEsRateInPes :: Lens' M2tsSettings (Maybe M2tsEsRateInPes)
msEsRateInPes = lens _msEsRateInPes (\ s a -> s{_msEsRateInPes = a})

-- | Packet Identifier (PID) for input source ETV Platform data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msEtvPlatformPid :: Lens' M2tsSettings (Maybe Text)
msEtvPlatformPid = lens _msEtvPlatformPid (\ s a -> s{_msEtvPlatformPid = a})

-- | The output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate.
msBitrate :: Lens' M2tsSettings (Maybe Natural)
msBitrate = lens _msBitrate (\ s a -> s{_msBitrate = a}) . mapping _Nat

-- | Optionally pass SCTE-35 signals from the input source to this output.
msScte35Control :: Lens' M2tsSettings (Maybe M2tsScte35Control)
msScte35Control = lens _msScte35Control (\ s a -> s{_msScte35Control = a})

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values. Each PID specified must be in the range of 32 (or 0x20)..8182 (or 0x1ff6).
msAudioPids :: Lens' M2tsSettings (Maybe Text)
msAudioPids = lens _msAudioPids (\ s a -> s{_msAudioPids = a})

-- | Packet Identifier (PID) for input source DVB Teletext data to this output. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msDvbTeletextPid :: Lens' M2tsSettings (Maybe Text)
msDvbTeletextPid = lens _msDvbTeletextPid (\ s a -> s{_msDvbTeletextPid = a})

-- | If set to passthrough, passes any EBIF data from the input source to this output.
msEbif :: Lens' M2tsSettings (Maybe M2tsEbifControl)
msEbif = lens _msEbif (\ s a -> s{_msEbif = a})

-- | When set to enabled, uses ARIB-compliant field muxing and removes video descriptor.
msArib :: Lens' M2tsSettings (Maybe M2tsArib)
msArib = lens _msArib (\ s a -> s{_msArib = a})

-- | Packet Identifier (PID) for ARIB Captions in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
msAribCaptionsPid :: Lens' M2tsSettings (Maybe Text)
msAribCaptionsPid = lens _msAribCaptionsPid (\ s a -> s{_msAribCaptionsPid = a})

-- | When set to drop, output audio streams will be removed from the program if the selected input audio stream is removed from the input. This allows the output audio configuration to dynamically change based on input configuration. If this is set to encodeSilence, all output audio streams will output encoded silence when not connected to an active input stream.
msAbsentInputAudioBehavior :: Lens' M2tsSettings (Maybe M2tsAbsentInputAudioBehavior)
msAbsentInputAudioBehavior = lens _msAbsentInputAudioBehavior (\ s a -> s{_msAbsentInputAudioBehavior = a})

-- | The length in seconds of each segment. Required unless markers is set to _none_.
msSegmentationTime :: Lens' M2tsSettings (Maybe Double)
msSegmentationTime = lens _msSegmentationTime (\ s a -> s{_msSegmentationTime = a})

-- | When videoAndFixedIntervals is selected, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. Only available when EBP Cablelabs segmentation markers are selected.  Partitions 1 and 2 will always follow the video interval.
msEbpAudioInterval :: Lens' M2tsSettings (Maybe M2tsAudioInterval)
msEbpAudioInterval = lens _msEbpAudioInterval (\ s a -> s{_msEbpAudioInterval = a})

-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
msDvbNitSettings :: Lens' M2tsSettings (Maybe DvbNitSettings)
msDvbNitSettings = lens _msDvbNitSettings (\ s a -> s{_msDvbNitSettings = a})

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
msPcrControl :: Lens' M2tsSettings (Maybe M2tsPcrControl)
msPcrControl = lens _msPcrControl (\ s a -> s{_msPcrControl = a})

-- | Controls placement of EBP on Audio PIDs. If set to videoAndAudioPids, EBP markers will be placed on the video PID and all audio PIDs.  If set to videoPid, EBP markers will be placed on only the video PID.
msEbpPlacement :: Lens' M2tsSettings (Maybe M2tsEbpPlacement)
msEbpPlacement = lens _msEbpPlacement (\ s a -> s{_msEbpPlacement = a})

-- | When vbr, does not insert null packets into transport stream to fill specified bitrate. The bitrate setting acts as the maximum bitrate when vbr is set.
msRateMode :: Lens' M2tsSettings (Maybe M2tsRateMode)
msRateMode = lens _msRateMode (\ s a -> s{_msRateMode = a})

-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "resetCadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of $segmentationTime seconds. When a segmentation style of "maintainCadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentationTime seconds. Note that EBP lookahead is a slight exception to this rule.
msSegmentationStyle :: Lens' M2tsSettings (Maybe M2tsSegmentationStyle)
msSegmentationStyle = lens _msSegmentationStyle (\ s a -> s{_msSegmentationStyle = a})

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
msDvbTdtSettings :: Lens' M2tsSettings (Maybe DvbTdtSettings)
msDvbTdtSettings = lens _msDvbTdtSettings (\ s a -> s{_msDvbTdtSettings = a})

instance FromJSON M2tsSettings where
        parseJSON
          = withObject "M2tsSettings"
              (\ x ->
                 M2tsSettings' <$>
                   (x .:? "pmtPid") <*> (x .:? "etvSignalPid") <*>
                     (x .:? "videoPid")
                     <*> (x .:? "nielsenId3Behavior")
                     <*> (x .:? "bufferModel")
                     <*> (x .:? "scte35Pid")
                     <*> (x .:? "transportStreamId")
                     <*> (x .:? "programNum")
                     <*> (x .:? "fragmentTime")
                     <*> (x .:? "timedMetadataBehavior")
                     <*> (x .:? "ccDescriptor")
                     <*> (x .:? "pmtInterval")
                     <*> (x .:? "dvbSdtSettings")
                     <*> (x .:? "ecmPid")
                     <*> (x .:? "nullPacketBitrate")
                     <*> (x .:? "audioBufferModel")
                     <*> (x .:? "timedMetadataPid")
                     <*> (x .:? "klv")
                     <*> (x .:? "audioFramesPerPes")
                     <*> (x .:? "pcrPeriod")
                     <*> (x .:? "pcrPid")
                     <*> (x .:? "segmentationMarkers")
                     <*> (x .:? "aribCaptionsPidControl")
                     <*> (x .:? "klvDataPids")
                     <*> (x .:? "ebpLookaheadMs")
                     <*> (x .:? "dvbSubPids")
                     <*> (x .:? "scte27Pids")
                     <*> (x .:? "patInterval")
                     <*> (x .:? "audioStreamType")
                     <*> (x .:? "esRateInPes")
                     <*> (x .:? "etvPlatformPid")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "scte35Control")
                     <*> (x .:? "audioPids")
                     <*> (x .:? "dvbTeletextPid")
                     <*> (x .:? "ebif")
                     <*> (x .:? "arib")
                     <*> (x .:? "aribCaptionsPid")
                     <*> (x .:? "absentInputAudioBehavior")
                     <*> (x .:? "segmentationTime")
                     <*> (x .:? "ebpAudioInterval")
                     <*> (x .:? "dvbNitSettings")
                     <*> (x .:? "pcrControl")
                     <*> (x .:? "ebpPlacement")
                     <*> (x .:? "rateMode")
                     <*> (x .:? "segmentationStyle")
                     <*> (x .:? "dvbTdtSettings"))

instance Hashable M2tsSettings where

instance NFData M2tsSettings where

instance ToJSON M2tsSettings where
        toJSON M2tsSettings'{..}
          = object
              (catMaybes
                 [("pmtPid" .=) <$> _msPmtPid,
                  ("etvSignalPid" .=) <$> _msEtvSignalPid,
                  ("videoPid" .=) <$> _msVideoPid,
                  ("nielsenId3Behavior" .=) <$> _msNielsenId3Behavior,
                  ("bufferModel" .=) <$> _msBufferModel,
                  ("scte35Pid" .=) <$> _msScte35Pid,
                  ("transportStreamId" .=) <$> _msTransportStreamId,
                  ("programNum" .=) <$> _msProgramNum,
                  ("fragmentTime" .=) <$> _msFragmentTime,
                  ("timedMetadataBehavior" .=) <$>
                    _msTimedMetadataBehavior,
                  ("ccDescriptor" .=) <$> _msCCDescriptor,
                  ("pmtInterval" .=) <$> _msPmtInterval,
                  ("dvbSdtSettings" .=) <$> _msDvbSdtSettings,
                  ("ecmPid" .=) <$> _msEcmPid,
                  ("nullPacketBitrate" .=) <$> _msNullPacketBitrate,
                  ("audioBufferModel" .=) <$> _msAudioBufferModel,
                  ("timedMetadataPid" .=) <$> _msTimedMetadataPid,
                  ("klv" .=) <$> _msKlv,
                  ("audioFramesPerPes" .=) <$> _msAudioFramesPerPes,
                  ("pcrPeriod" .=) <$> _msPcrPeriod,
                  ("pcrPid" .=) <$> _msPcrPid,
                  ("segmentationMarkers" .=) <$>
                    _msSegmentationMarkers,
                  ("aribCaptionsPidControl" .=) <$>
                    _msAribCaptionsPidControl,
                  ("klvDataPids" .=) <$> _msKlvDataPids,
                  ("ebpLookaheadMs" .=) <$> _msEbpLookaheadMs,
                  ("dvbSubPids" .=) <$> _msDvbSubPids,
                  ("scte27Pids" .=) <$> _msScte27Pids,
                  ("patInterval" .=) <$> _msPatInterval,
                  ("audioStreamType" .=) <$> _msAudioStreamType,
                  ("esRateInPes" .=) <$> _msEsRateInPes,
                  ("etvPlatformPid" .=) <$> _msEtvPlatformPid,
                  ("bitrate" .=) <$> _msBitrate,
                  ("scte35Control" .=) <$> _msScte35Control,
                  ("audioPids" .=) <$> _msAudioPids,
                  ("dvbTeletextPid" .=) <$> _msDvbTeletextPid,
                  ("ebif" .=) <$> _msEbif, ("arib" .=) <$> _msArib,
                  ("aribCaptionsPid" .=) <$> _msAribCaptionsPid,
                  ("absentInputAudioBehavior" .=) <$>
                    _msAbsentInputAudioBehavior,
                  ("segmentationTime" .=) <$> _msSegmentationTime,
                  ("ebpAudioInterval" .=) <$> _msEbpAudioInterval,
                  ("dvbNitSettings" .=) <$> _msDvbNitSettings,
                  ("pcrControl" .=) <$> _msPcrControl,
                  ("ebpPlacement" .=) <$> _msEbpPlacement,
                  ("rateMode" .=) <$> _msRateMode,
                  ("segmentationStyle" .=) <$> _msSegmentationStyle,
                  ("dvbTdtSettings" .=) <$> _msDvbTdtSettings])

-- | Settings information for the .m3u8 container
--
-- /See:/ 'm3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'{_mPmtPid ::
                                  !(Maybe Text),
                                  _mVideoPid :: !(Maybe Text),
                                  _mNielsenId3Behavior ::
                                  !(Maybe M3u8NielsenId3Behavior),
                                  _mScte35Pid :: !(Maybe Text),
                                  _mTransportStreamId :: !(Maybe Nat),
                                  _mProgramNum :: !(Maybe Nat),
                                  _mTimedMetadataBehavior ::
                                  !(Maybe M3u8TimedMetadataBehavior),
                                  _mPmtInterval :: !(Maybe Nat),
                                  _mEcmPid :: !(Maybe Text),
                                  _mTimedMetadataPid :: !(Maybe Text),
                                  _mAudioFramesPerPes :: !(Maybe Nat),
                                  _mPcrPeriod :: !(Maybe Nat),
                                  _mPcrPid :: !(Maybe Text),
                                  _mPatInterval :: !(Maybe Nat),
                                  _mAudioPids :: !(Maybe Text),
                                  _mScte35Behavior ::
                                  !(Maybe M3u8Scte35Behavior),
                                  _mPcrControl :: !(Maybe M3u8PcrControl)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'M3u8Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mPmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- * 'mVideoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- * 'mNielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- * 'mScte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- * 'mTransportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- * 'mProgramNum' - The value of the program number field in the Program Map Table.
--
-- * 'mTimedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to output.
--
-- * 'mPmtInterval' - The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- * 'mEcmPid' - This parameter is unused and deprecated.
--
-- * 'mTimedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'mAudioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- * 'mPcrPeriod' - Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
--
-- * 'mPcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
--
-- * 'mPatInterval' - The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- * 'mAudioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
--
-- * 'mScte35Behavior' - If set to passthrough, passes any SCTE-35 signals from the input source to this output.
--
-- * 'mPcrControl' - When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
m3u8Settings
    :: M3u8Settings
m3u8Settings
  = M3u8Settings'{_mPmtPid = Nothing,
                  _mVideoPid = Nothing, _mNielsenId3Behavior = Nothing,
                  _mScte35Pid = Nothing, _mTransportStreamId = Nothing,
                  _mProgramNum = Nothing,
                  _mTimedMetadataBehavior = Nothing,
                  _mPmtInterval = Nothing, _mEcmPid = Nothing,
                  _mTimedMetadataPid = Nothing,
                  _mAudioFramesPerPes = Nothing, _mPcrPeriod = Nothing,
                  _mPcrPid = Nothing, _mPatInterval = Nothing,
                  _mAudioPids = Nothing, _mScte35Behavior = Nothing,
                  _mPcrControl = Nothing}

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
mPmtPid :: Lens' M3u8Settings (Maybe Text)
mPmtPid = lens _mPmtPid (\ s a -> s{_mPmtPid = a})

-- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
mVideoPid :: Lens' M3u8Settings (Maybe Text)
mVideoPid = lens _mVideoPid (\ s a -> s{_mVideoPid = a})

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
mNielsenId3Behavior :: Lens' M3u8Settings (Maybe M3u8NielsenId3Behavior)
mNielsenId3Behavior = lens _mNielsenId3Behavior (\ s a -> s{_mNielsenId3Behavior = a})

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
mScte35Pid :: Lens' M3u8Settings (Maybe Text)
mScte35Pid = lens _mScte35Pid (\ s a -> s{_mScte35Pid = a})

-- | The value of the transport stream ID field in the Program Map Table.
mTransportStreamId :: Lens' M3u8Settings (Maybe Natural)
mTransportStreamId = lens _mTransportStreamId (\ s a -> s{_mTransportStreamId = a}) . mapping _Nat

-- | The value of the program number field in the Program Map Table.
mProgramNum :: Lens' M3u8Settings (Maybe Natural)
mProgramNum = lens _mProgramNum (\ s a -> s{_mProgramNum = a}) . mapping _Nat

-- | When set to passthrough, timed metadata is passed through from input to output.
mTimedMetadataBehavior :: Lens' M3u8Settings (Maybe M3u8TimedMetadataBehavior)
mTimedMetadataBehavior = lens _mTimedMetadataBehavior (\ s a -> s{_mTimedMetadataBehavior = a})

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
mPmtInterval :: Lens' M3u8Settings (Maybe Natural)
mPmtInterval = lens _mPmtInterval (\ s a -> s{_mPmtInterval = a}) . mapping _Nat

-- | This parameter is unused and deprecated.
mEcmPid :: Lens' M3u8Settings (Maybe Text)
mEcmPid = lens _mEcmPid (\ s a -> s{_mEcmPid = a})

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
mTimedMetadataPid :: Lens' M3u8Settings (Maybe Text)
mTimedMetadataPid = lens _mTimedMetadataPid (\ s a -> s{_mTimedMetadataPid = a})

-- | The number of audio frames to insert for each PES packet.
mAudioFramesPerPes :: Lens' M3u8Settings (Maybe Natural)
mAudioFramesPerPes = lens _mAudioFramesPerPes (\ s a -> s{_mAudioFramesPerPes = a}) . mapping _Nat

-- | Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
mPcrPeriod :: Lens' M3u8Settings (Maybe Natural)
mPcrPeriod = lens _mPcrPeriod (\ s a -> s{_mPcrPeriod = a}) . mapping _Nat

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
mPcrPid :: Lens' M3u8Settings (Maybe Text)
mPcrPid = lens _mPcrPid (\ s a -> s{_mPcrPid = a})

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
mPatInterval :: Lens' M3u8Settings (Maybe Natural)
mPatInterval = lens _mPatInterval (\ s a -> s{_mPatInterval = a}) . mapping _Nat

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
mAudioPids :: Lens' M3u8Settings (Maybe Text)
mAudioPids = lens _mAudioPids (\ s a -> s{_mAudioPids = a})

-- | If set to passthrough, passes any SCTE-35 signals from the input source to this output.
mScte35Behavior :: Lens' M3u8Settings (Maybe M3u8Scte35Behavior)
mScte35Behavior = lens _mScte35Behavior (\ s a -> s{_mScte35Behavior = a})

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
mPcrControl :: Lens' M3u8Settings (Maybe M3u8PcrControl)
mPcrControl = lens _mPcrControl (\ s a -> s{_mPcrControl = a})

instance FromJSON M3u8Settings where
        parseJSON
          = withObject "M3u8Settings"
              (\ x ->
                 M3u8Settings' <$>
                   (x .:? "pmtPid") <*> (x .:? "videoPid") <*>
                     (x .:? "nielsenId3Behavior")
                     <*> (x .:? "scte35Pid")
                     <*> (x .:? "transportStreamId")
                     <*> (x .:? "programNum")
                     <*> (x .:? "timedMetadataBehavior")
                     <*> (x .:? "pmtInterval")
                     <*> (x .:? "ecmPid")
                     <*> (x .:? "timedMetadataPid")
                     <*> (x .:? "audioFramesPerPes")
                     <*> (x .:? "pcrPeriod")
                     <*> (x .:? "pcrPid")
                     <*> (x .:? "patInterval")
                     <*> (x .:? "audioPids")
                     <*> (x .:? "scte35Behavior")
                     <*> (x .:? "pcrControl"))

instance Hashable M3u8Settings where

instance NFData M3u8Settings where

instance ToJSON M3u8Settings where
        toJSON M3u8Settings'{..}
          = object
              (catMaybes
                 [("pmtPid" .=) <$> _mPmtPid,
                  ("videoPid" .=) <$> _mVideoPid,
                  ("nielsenId3Behavior" .=) <$> _mNielsenId3Behavior,
                  ("scte35Pid" .=) <$> _mScte35Pid,
                  ("transportStreamId" .=) <$> _mTransportStreamId,
                  ("programNum" .=) <$> _mProgramNum,
                  ("timedMetadataBehavior" .=) <$>
                    _mTimedMetadataBehavior,
                  ("pmtInterval" .=) <$> _mPmtInterval,
                  ("ecmPid" .=) <$> _mEcmPid,
                  ("timedMetadataPid" .=) <$> _mTimedMetadataPid,
                  ("audioFramesPerPes" .=) <$> _mAudioFramesPerPes,
                  ("pcrPeriod" .=) <$> _mPcrPeriod,
                  ("pcrPid" .=) <$> _mPcrPid,
                  ("patInterval" .=) <$> _mPatInterval,
                  ("audioPids" .=) <$> _mAudioPids,
                  ("scte35Behavior" .=) <$> _mScte35Behavior,
                  ("pcrControl" .=) <$> _mPcrControl])

-- | The settings for a MediaConnect Flow.
--
-- /See:/ 'mediaConnectFlow' smart constructor.
newtype MediaConnectFlow = MediaConnectFlow'{_mcfFlowARN
                                             :: Maybe Text}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MediaConnectFlow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcfFlowARN' - The unique ARN of the MediaConnect Flow being used as a source.
mediaConnectFlow
    :: MediaConnectFlow
mediaConnectFlow
  = MediaConnectFlow'{_mcfFlowARN = Nothing}

-- | The unique ARN of the MediaConnect Flow being used as a source.
mcfFlowARN :: Lens' MediaConnectFlow (Maybe Text)
mcfFlowARN = lens _mcfFlowARN (\ s a -> s{_mcfFlowARN = a})

instance FromJSON MediaConnectFlow where
        parseJSON
          = withObject "MediaConnectFlow"
              (\ x -> MediaConnectFlow' <$> (x .:? "flowArn"))

instance Hashable MediaConnectFlow where

instance NFData MediaConnectFlow where

-- | The settings for a MediaConnect Flow.
--
-- /See:/ 'mediaConnectFlowRequest' smart constructor.
newtype MediaConnectFlowRequest = MediaConnectFlowRequest'{_mcfrFlowARN
                                                           :: Maybe Text}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'MediaConnectFlowRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcfrFlowARN' - The ARN of the MediaConnect Flow that you want to use as a source.
mediaConnectFlowRequest
    :: MediaConnectFlowRequest
mediaConnectFlowRequest
  = MediaConnectFlowRequest'{_mcfrFlowARN = Nothing}

-- | The ARN of the MediaConnect Flow that you want to use as a source.
mcfrFlowARN :: Lens' MediaConnectFlowRequest (Maybe Text)
mcfrFlowARN = lens _mcfrFlowARN (\ s a -> s{_mcfrFlowARN = a})

instance Hashable MediaConnectFlowRequest where

instance NFData MediaConnectFlowRequest where

instance ToJSON MediaConnectFlowRequest where
        toJSON MediaConnectFlowRequest'{..}
          = object
              (catMaybes [("flowArn" .=) <$> _mcfrFlowARN])

-- | Media Package Group Settings
--
-- /See:/ 'mediaPackageGroupSettings' smart constructor.
newtype MediaPackageGroupSettings = MediaPackageGroupSettings'{_mpgsDestination
                                                               ::
                                                               OutputLocationRef}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'MediaPackageGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpgsDestination' - MediaPackage channel destination.
mediaPackageGroupSettings
    :: OutputLocationRef -- ^ 'mpgsDestination'
    -> MediaPackageGroupSettings
mediaPackageGroupSettings pDestination_
  = MediaPackageGroupSettings'{_mpgsDestination =
                                 pDestination_}

-- | MediaPackage channel destination.
mpgsDestination :: Lens' MediaPackageGroupSettings OutputLocationRef
mpgsDestination = lens _mpgsDestination (\ s a -> s{_mpgsDestination = a})

instance FromJSON MediaPackageGroupSettings where
        parseJSON
          = withObject "MediaPackageGroupSettings"
              (\ x ->
                 MediaPackageGroupSettings' <$> (x .: "destination"))

instance Hashable MediaPackageGroupSettings where

instance NFData MediaPackageGroupSettings where

instance ToJSON MediaPackageGroupSettings where
        toJSON MediaPackageGroupSettings'{..}
          = object
              (catMaybes
                 [Just ("destination" .= _mpgsDestination)])

-- | MediaPackage Output Destination Settings
--
-- /See:/ 'mediaPackageOutputDestinationSettings' smart constructor.
newtype MediaPackageOutputDestinationSettings = MediaPackageOutputDestinationSettings'{_mpodsChannelId
                                                                                       ::
                                                                                       Maybe
                                                                                         Text}
                                                  deriving (Eq, Read, Show,
                                                            Data, Typeable,
                                                            Generic)

-- | Creates a value of 'MediaPackageOutputDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpodsChannelId' - ID of the channel in MediaPackage that is the destination for this output group. You do not need to specify the individual inputs in MediaPackage; MediaLive will handle the connection of the two MediaLive pipelines to the two MediaPackage inputs. The MediaPackage channel and MediaLive channel must be in the same region.
mediaPackageOutputDestinationSettings
    :: MediaPackageOutputDestinationSettings
mediaPackageOutputDestinationSettings
  = MediaPackageOutputDestinationSettings'{_mpodsChannelId
                                             = Nothing}

-- | ID of the channel in MediaPackage that is the destination for this output group. You do not need to specify the individual inputs in MediaPackage; MediaLive will handle the connection of the two MediaLive pipelines to the two MediaPackage inputs. The MediaPackage channel and MediaLive channel must be in the same region.
mpodsChannelId :: Lens' MediaPackageOutputDestinationSettings (Maybe Text)
mpodsChannelId = lens _mpodsChannelId (\ s a -> s{_mpodsChannelId = a})

instance FromJSON
           MediaPackageOutputDestinationSettings
         where
        parseJSON
          = withObject "MediaPackageOutputDestinationSettings"
              (\ x ->
                 MediaPackageOutputDestinationSettings' <$>
                   (x .:? "channelId"))

instance Hashable
           MediaPackageOutputDestinationSettings
         where

instance NFData MediaPackageOutputDestinationSettings
         where

instance ToJSON MediaPackageOutputDestinationSettings
         where
        toJSON MediaPackageOutputDestinationSettings'{..}
          = object
              (catMaybes [("channelId" .=) <$> _mpodsChannelId])

-- | Media Package Output Settings
--
-- /See:/ 'mediaPackageOutputSettings' smart constructor.
data MediaPackageOutputSettings = MediaPackageOutputSettings'
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'MediaPackageOutputSettings' with the minimum fields required to make a request.
--
mediaPackageOutputSettings
    :: MediaPackageOutputSettings
mediaPackageOutputSettings
  = MediaPackageOutputSettings'

instance FromJSON MediaPackageOutputSettings where
        parseJSON
          = withObject "MediaPackageOutputSettings"
              (\ x -> pure MediaPackageOutputSettings')

instance Hashable MediaPackageOutputSettings where

instance NFData MediaPackageOutputSettings where

instance ToJSON MediaPackageOutputSettings where
        toJSON = const (Object mempty)

-- | Mp2 Settings
--
-- /See:/ 'mp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'{_mCodingMode ::
                                !(Maybe Mp2CodingMode),
                                _mSampleRate :: !(Maybe Double),
                                _mBitrate :: !(Maybe Double)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Mp2Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mCodingMode' - The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
--
-- * 'mSampleRate' - Sample rate in Hz.
--
-- * 'mBitrate' - Average bitrate in bits/second.
mp2Settings
    :: Mp2Settings
mp2Settings
  = Mp2Settings'{_mCodingMode = Nothing,
                 _mSampleRate = Nothing, _mBitrate = Nothing}

-- | The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
mCodingMode :: Lens' Mp2Settings (Maybe Mp2CodingMode)
mCodingMode = lens _mCodingMode (\ s a -> s{_mCodingMode = a})

-- | Sample rate in Hz.
mSampleRate :: Lens' Mp2Settings (Maybe Double)
mSampleRate = lens _mSampleRate (\ s a -> s{_mSampleRate = a})

-- | Average bitrate in bits/second.
mBitrate :: Lens' Mp2Settings (Maybe Double)
mBitrate = lens _mBitrate (\ s a -> s{_mBitrate = a})

instance FromJSON Mp2Settings where
        parseJSON
          = withObject "Mp2Settings"
              (\ x ->
                 Mp2Settings' <$>
                   (x .:? "codingMode") <*> (x .:? "sampleRate") <*>
                     (x .:? "bitrate"))

instance Hashable Mp2Settings where

instance NFData Mp2Settings where

instance ToJSON Mp2Settings where
        toJSON Mp2Settings'{..}
          = object
              (catMaybes
                 [("codingMode" .=) <$> _mCodingMode,
                  ("sampleRate" .=) <$> _mSampleRate,
                  ("bitrate" .=) <$> _mBitrate])

-- | Ms Smooth Group Settings
--
-- /See:/ 'msSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'{_msgsFragmentLength
                                                    :: !(Maybe Nat),
                                                    _msgsStreamManifestBehavior
                                                    ::
                                                    !(Maybe
                                                        SmoothGroupStreamManifestBehavior),
                                                    _msgsSendDelayMs ::
                                                    !(Maybe Nat),
                                                    _msgsEventStopBehavior ::
                                                    !(Maybe
                                                        SmoothGroupEventStopBehavior),
                                                    _msgsTimestampOffsetMode ::
                                                    !(Maybe
                                                        SmoothGroupTimestampOffsetMode),
                                                    _msgsNumRetries ::
                                                    !(Maybe Nat),
                                                    _msgsAcquisitionPointId ::
                                                    !(Maybe Text),
                                                    _msgsInputLossAction ::
                                                    !(Maybe
                                                        InputLossActionForMsSmoothOut),
                                                    _msgsTimestampOffset ::
                                                    !(Maybe Text),
                                                    _msgsCertificateMode ::
                                                    !(Maybe
                                                        SmoothGroupCertificateMode),
                                                    _msgsSparseTrackType ::
                                                    !(Maybe
                                                        SmoothGroupSparseTrackType),
                                                    _msgsConnectionRetryInterval
                                                    :: !(Maybe Nat),
                                                    _msgsFilecacheDuration ::
                                                    !(Maybe Nat),
                                                    _msgsRestartDelay ::
                                                    !(Maybe Nat),
                                                    _msgsEventIdMode ::
                                                    !(Maybe
                                                        SmoothGroupEventIdMode),
                                                    _msgsAudioOnlyTimecodeControl
                                                    ::
                                                    !(Maybe
                                                        SmoothGroupAudioOnlyTimecodeControl),
                                                    _msgsSegmentationMode ::
                                                    !(Maybe
                                                        SmoothGroupSegmentationMode),
                                                    _msgsEventId ::
                                                    !(Maybe Text),
                                                    _msgsDestination ::
                                                    !OutputLocationRef}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'MsSmoothGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msgsFragmentLength' - Length of mp4 fragments to generate (in seconds). Fragment length must be compatible with GOP size and framerate.
--
-- * 'msgsStreamManifestBehavior' - When set to send, send stream manifest so publishing point doesn't start until all streams start.
--
-- * 'msgsSendDelayMs' - Number of milliseconds to delay the output from the second pipeline.
--
-- * 'msgsEventStopBehavior' - When set to sendEos, send EOS signal to IIS server when stopping the event
--
-- * 'msgsTimestampOffsetMode' - Type of timestamp date offset to use. - useEventStartDate: Use the date the event was started as the offset - useConfiguredOffset: Use an explicitly configured date as the offset
--
-- * 'msgsNumRetries' - Number of retry attempts.
--
-- * 'msgsAcquisitionPointId' - The ID to include in each message in the sparse track. Ignored if sparseTrackType is NONE.
--
-- * 'msgsInputLossAction' - Parameter that control output group behavior on input loss.
--
-- * 'msgsTimestampOffset' - Timestamp offset for the event.  Only used if timestampOffsetMode is set to useConfiguredOffset.
--
-- * 'msgsCertificateMode' - If set to verifyAuthenticity, verify the https certificate chain to a trusted Certificate Authority (CA).  This will cause https outputs to self-signed certificates to fail.
--
-- * 'msgsSparseTrackType' - Identifies the type of data to place in the sparse track: - SCTE35: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame to start a new segment. - SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame but don't start a new segment. - NONE: Don't generate a sparse track for any outputs in this output group.
--
-- * 'msgsConnectionRetryInterval' - Number of seconds to wait before retrying connection to the IIS server if the connection is lost. Content will be cached during this time and the cache will be be delivered to the IIS server once the connection is re-established.
--
-- * 'msgsFilecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- * 'msgsRestartDelay' - Number of seconds before initiating a restart due to output failure, due to exhausting the numRetries on one segment, or exceeding filecacheDuration.
--
-- * 'msgsEventIdMode' - Specifies whether or not to send an event ID to the IIS server. If no event ID is sent and the same Live Event is used without changing the publishing point, clients might see cached video from the previous run. Options: - "useConfigured" - use the value provided in eventId - "useTimestamp" - generate and send an event ID based on the current timestamp - "noEventId" - do not send an event ID to the IIS server.
--
-- * 'msgsAudioOnlyTimecodeControl' - If set to passthrough for an audio-only MS Smooth output, the fragment absolute time will be set to the current timecode. This option does not write timecodes to the audio elementary stream.
--
-- * 'msgsSegmentationMode' - useInputSegmentation has been deprecated. The configured segment size is always used.
--
-- * 'msgsEventId' - MS Smooth event ID to be sent to the IIS server. Should only be specified if eventIdMode is set to useConfigured.
--
-- * 'msgsDestination' - Smooth Streaming publish point on an IIS server. Elemental Live acts as a "Push" encoder to IIS.
msSmoothGroupSettings
    :: OutputLocationRef -- ^ 'msgsDestination'
    -> MsSmoothGroupSettings
msSmoothGroupSettings pDestination_
  = MsSmoothGroupSettings'{_msgsFragmentLength =
                             Nothing,
                           _msgsStreamManifestBehavior = Nothing,
                           _msgsSendDelayMs = Nothing,
                           _msgsEventStopBehavior = Nothing,
                           _msgsTimestampOffsetMode = Nothing,
                           _msgsNumRetries = Nothing,
                           _msgsAcquisitionPointId = Nothing,
                           _msgsInputLossAction = Nothing,
                           _msgsTimestampOffset = Nothing,
                           _msgsCertificateMode = Nothing,
                           _msgsSparseTrackType = Nothing,
                           _msgsConnectionRetryInterval = Nothing,
                           _msgsFilecacheDuration = Nothing,
                           _msgsRestartDelay = Nothing,
                           _msgsEventIdMode = Nothing,
                           _msgsAudioOnlyTimecodeControl = Nothing,
                           _msgsSegmentationMode = Nothing,
                           _msgsEventId = Nothing,
                           _msgsDestination = pDestination_}

-- | Length of mp4 fragments to generate (in seconds). Fragment length must be compatible with GOP size and framerate.
msgsFragmentLength :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsFragmentLength = lens _msgsFragmentLength (\ s a -> s{_msgsFragmentLength = a}) . mapping _Nat

-- | When set to send, send stream manifest so publishing point doesn't start until all streams start.
msgsStreamManifestBehavior :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupStreamManifestBehavior)
msgsStreamManifestBehavior = lens _msgsStreamManifestBehavior (\ s a -> s{_msgsStreamManifestBehavior = a})

-- | Number of milliseconds to delay the output from the second pipeline.
msgsSendDelayMs :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsSendDelayMs = lens _msgsSendDelayMs (\ s a -> s{_msgsSendDelayMs = a}) . mapping _Nat

-- | When set to sendEos, send EOS signal to IIS server when stopping the event
msgsEventStopBehavior :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupEventStopBehavior)
msgsEventStopBehavior = lens _msgsEventStopBehavior (\ s a -> s{_msgsEventStopBehavior = a})

-- | Type of timestamp date offset to use. - useEventStartDate: Use the date the event was started as the offset - useConfiguredOffset: Use an explicitly configured date as the offset
msgsTimestampOffsetMode :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupTimestampOffsetMode)
msgsTimestampOffsetMode = lens _msgsTimestampOffsetMode (\ s a -> s{_msgsTimestampOffsetMode = a})

-- | Number of retry attempts.
msgsNumRetries :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsNumRetries = lens _msgsNumRetries (\ s a -> s{_msgsNumRetries = a}) . mapping _Nat

-- | The ID to include in each message in the sparse track. Ignored if sparseTrackType is NONE.
msgsAcquisitionPointId :: Lens' MsSmoothGroupSettings (Maybe Text)
msgsAcquisitionPointId = lens _msgsAcquisitionPointId (\ s a -> s{_msgsAcquisitionPointId = a})

-- | Parameter that control output group behavior on input loss.
msgsInputLossAction :: Lens' MsSmoothGroupSettings (Maybe InputLossActionForMsSmoothOut)
msgsInputLossAction = lens _msgsInputLossAction (\ s a -> s{_msgsInputLossAction = a})

-- | Timestamp offset for the event.  Only used if timestampOffsetMode is set to useConfiguredOffset.
msgsTimestampOffset :: Lens' MsSmoothGroupSettings (Maybe Text)
msgsTimestampOffset = lens _msgsTimestampOffset (\ s a -> s{_msgsTimestampOffset = a})

-- | If set to verifyAuthenticity, verify the https certificate chain to a trusted Certificate Authority (CA).  This will cause https outputs to self-signed certificates to fail.
msgsCertificateMode :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupCertificateMode)
msgsCertificateMode = lens _msgsCertificateMode (\ s a -> s{_msgsCertificateMode = a})

-- | Identifies the type of data to place in the sparse track: - SCTE35: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame to start a new segment. - SCTE35_WITHOUT_SEGMENTATION: Insert SCTE-35 messages from the source content. With each message, insert an IDR frame but don't start a new segment. - NONE: Don't generate a sparse track for any outputs in this output group.
msgsSparseTrackType :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupSparseTrackType)
msgsSparseTrackType = lens _msgsSparseTrackType (\ s a -> s{_msgsSparseTrackType = a})

-- | Number of seconds to wait before retrying connection to the IIS server if the connection is lost. Content will be cached during this time and the cache will be be delivered to the IIS server once the connection is re-established.
msgsConnectionRetryInterval :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsConnectionRetryInterval = lens _msgsConnectionRetryInterval (\ s a -> s{_msgsConnectionRetryInterval = a}) . mapping _Nat

-- | Size in seconds of file cache for streaming outputs.
msgsFilecacheDuration :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsFilecacheDuration = lens _msgsFilecacheDuration (\ s a -> s{_msgsFilecacheDuration = a}) . mapping _Nat

-- | Number of seconds before initiating a restart due to output failure, due to exhausting the numRetries on one segment, or exceeding filecacheDuration.
msgsRestartDelay :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsRestartDelay = lens _msgsRestartDelay (\ s a -> s{_msgsRestartDelay = a}) . mapping _Nat

-- | Specifies whether or not to send an event ID to the IIS server. If no event ID is sent and the same Live Event is used without changing the publishing point, clients might see cached video from the previous run. Options: - "useConfigured" - use the value provided in eventId - "useTimestamp" - generate and send an event ID based on the current timestamp - "noEventId" - do not send an event ID to the IIS server.
msgsEventIdMode :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupEventIdMode)
msgsEventIdMode = lens _msgsEventIdMode (\ s a -> s{_msgsEventIdMode = a})

-- | If set to passthrough for an audio-only MS Smooth output, the fragment absolute time will be set to the current timecode. This option does not write timecodes to the audio elementary stream.
msgsAudioOnlyTimecodeControl :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupAudioOnlyTimecodeControl)
msgsAudioOnlyTimecodeControl = lens _msgsAudioOnlyTimecodeControl (\ s a -> s{_msgsAudioOnlyTimecodeControl = a})

-- | useInputSegmentation has been deprecated. The configured segment size is always used.
msgsSegmentationMode :: Lens' MsSmoothGroupSettings (Maybe SmoothGroupSegmentationMode)
msgsSegmentationMode = lens _msgsSegmentationMode (\ s a -> s{_msgsSegmentationMode = a})

-- | MS Smooth event ID to be sent to the IIS server. Should only be specified if eventIdMode is set to useConfigured.
msgsEventId :: Lens' MsSmoothGroupSettings (Maybe Text)
msgsEventId = lens _msgsEventId (\ s a -> s{_msgsEventId = a})

-- | Smooth Streaming publish point on an IIS server. Elemental Live acts as a "Push" encoder to IIS.
msgsDestination :: Lens' MsSmoothGroupSettings OutputLocationRef
msgsDestination = lens _msgsDestination (\ s a -> s{_msgsDestination = a})

instance FromJSON MsSmoothGroupSettings where
        parseJSON
          = withObject "MsSmoothGroupSettings"
              (\ x ->
                 MsSmoothGroupSettings' <$>
                   (x .:? "fragmentLength") <*>
                     (x .:? "streamManifestBehavior")
                     <*> (x .:? "sendDelayMs")
                     <*> (x .:? "eventStopBehavior")
                     <*> (x .:? "timestampOffsetMode")
                     <*> (x .:? "numRetries")
                     <*> (x .:? "acquisitionPointId")
                     <*> (x .:? "inputLossAction")
                     <*> (x .:? "timestampOffset")
                     <*> (x .:? "certificateMode")
                     <*> (x .:? "sparseTrackType")
                     <*> (x .:? "connectionRetryInterval")
                     <*> (x .:? "filecacheDuration")
                     <*> (x .:? "restartDelay")
                     <*> (x .:? "eventIdMode")
                     <*> (x .:? "audioOnlyTimecodeControl")
                     <*> (x .:? "segmentationMode")
                     <*> (x .:? "eventId")
                     <*> (x .: "destination"))

instance Hashable MsSmoothGroupSettings where

instance NFData MsSmoothGroupSettings where

instance ToJSON MsSmoothGroupSettings where
        toJSON MsSmoothGroupSettings'{..}
          = object
              (catMaybes
                 [("fragmentLength" .=) <$> _msgsFragmentLength,
                  ("streamManifestBehavior" .=) <$>
                    _msgsStreamManifestBehavior,
                  ("sendDelayMs" .=) <$> _msgsSendDelayMs,
                  ("eventStopBehavior" .=) <$> _msgsEventStopBehavior,
                  ("timestampOffsetMode" .=) <$>
                    _msgsTimestampOffsetMode,
                  ("numRetries" .=) <$> _msgsNumRetries,
                  ("acquisitionPointId" .=) <$>
                    _msgsAcquisitionPointId,
                  ("inputLossAction" .=) <$> _msgsInputLossAction,
                  ("timestampOffset" .=) <$> _msgsTimestampOffset,
                  ("certificateMode" .=) <$> _msgsCertificateMode,
                  ("sparseTrackType" .=) <$> _msgsSparseTrackType,
                  ("connectionRetryInterval" .=) <$>
                    _msgsConnectionRetryInterval,
                  ("filecacheDuration" .=) <$> _msgsFilecacheDuration,
                  ("restartDelay" .=) <$> _msgsRestartDelay,
                  ("eventIdMode" .=) <$> _msgsEventIdMode,
                  ("audioOnlyTimecodeControl" .=) <$>
                    _msgsAudioOnlyTimecodeControl,
                  ("segmentationMode" .=) <$> _msgsSegmentationMode,
                  ("eventId" .=) <$> _msgsEventId,
                  Just ("destination" .= _msgsDestination)])

-- | Ms Smooth Output Settings
--
-- /See:/ 'msSmoothOutputSettings' smart constructor.
data MsSmoothOutputSettings = MsSmoothOutputSettings'{_msosH265PackagingType
                                                      ::
                                                      !(Maybe
                                                          MsSmoothH265PackagingType),
                                                      _msosNameModifier ::
                                                      !(Maybe Text)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'MsSmoothOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msosH265PackagingType' - Only applicable when this output is referencing an H.265 video description. Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
--
-- * 'msosNameModifier' - String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
msSmoothOutputSettings
    :: MsSmoothOutputSettings
msSmoothOutputSettings
  = MsSmoothOutputSettings'{_msosH265PackagingType =
                              Nothing,
                            _msosNameModifier = Nothing}

-- | Only applicable when this output is referencing an H.265 video description. Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
msosH265PackagingType :: Lens' MsSmoothOutputSettings (Maybe MsSmoothH265PackagingType)
msosH265PackagingType = lens _msosH265PackagingType (\ s a -> s{_msosH265PackagingType = a})

-- | String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
msosNameModifier :: Lens' MsSmoothOutputSettings (Maybe Text)
msosNameModifier = lens _msosNameModifier (\ s a -> s{_msosNameModifier = a})

instance FromJSON MsSmoothOutputSettings where
        parseJSON
          = withObject "MsSmoothOutputSettings"
              (\ x ->
                 MsSmoothOutputSettings' <$>
                   (x .:? "h265PackagingType") <*>
                     (x .:? "nameModifier"))

instance Hashable MsSmoothOutputSettings where

instance NFData MsSmoothOutputSettings where

instance ToJSON MsSmoothOutputSettings where
        toJSON MsSmoothOutputSettings'{..}
          = object
              (catMaybes
                 [("h265PackagingType" .=) <$> _msosH265PackagingType,
                  ("nameModifier" .=) <$> _msosNameModifier])

-- | The multiplex object.
--
-- /See:/ 'multiplex' smart constructor.
data Multiplex = Multiplex'{_mState ::
                            !(Maybe MultiplexState),
                            _mARN :: !(Maybe Text),
                            _mPipelinesRunningCount :: !(Maybe Int),
                            _mAvailabilityZones :: !(Maybe [Text]),
                            _mProgramCount :: !(Maybe Int),
                            _mDestinations ::
                            !(Maybe [MultiplexOutputDestination]),
                            _mName :: !(Maybe Text), _mId :: !(Maybe Text),
                            _mMultiplexSettings :: !(Maybe MultiplexSettings),
                            _mTags :: !(Maybe (Map Text Text))}
                   deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Multiplex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mState' - The current state of the multiplex.
--
-- * 'mARN' - The unique arn of the multiplex.
--
-- * 'mPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'mAvailabilityZones' - A list of availability zones for the multiplex.
--
-- * 'mProgramCount' - The number of programs in the multiplex.
--
-- * 'mDestinations' - A list of the multiplex output destinations.
--
-- * 'mName' - The name of the multiplex.
--
-- * 'mId' - The unique id of the multiplex.
--
-- * 'mMultiplexSettings' - Configuration for a multiplex event.
--
-- * 'mTags' - A collection of key-value pairs.
multiplex
    :: Multiplex
multiplex
  = Multiplex'{_mState = Nothing, _mARN = Nothing,
               _mPipelinesRunningCount = Nothing,
               _mAvailabilityZones = Nothing,
               _mProgramCount = Nothing, _mDestinations = Nothing,
               _mName = Nothing, _mId = Nothing,
               _mMultiplexSettings = Nothing, _mTags = Nothing}

-- | The current state of the multiplex.
mState :: Lens' Multiplex (Maybe MultiplexState)
mState = lens _mState (\ s a -> s{_mState = a})

-- | The unique arn of the multiplex.
mARN :: Lens' Multiplex (Maybe Text)
mARN = lens _mARN (\ s a -> s{_mARN = a})

-- | The number of currently healthy pipelines.
mPipelinesRunningCount :: Lens' Multiplex (Maybe Int)
mPipelinesRunningCount = lens _mPipelinesRunningCount (\ s a -> s{_mPipelinesRunningCount = a})

-- | A list of availability zones for the multiplex.
mAvailabilityZones :: Lens' Multiplex [Text]
mAvailabilityZones = lens _mAvailabilityZones (\ s a -> s{_mAvailabilityZones = a}) . _Default . _Coerce

-- | The number of programs in the multiplex.
mProgramCount :: Lens' Multiplex (Maybe Int)
mProgramCount = lens _mProgramCount (\ s a -> s{_mProgramCount = a})

-- | A list of the multiplex output destinations.
mDestinations :: Lens' Multiplex [MultiplexOutputDestination]
mDestinations = lens _mDestinations (\ s a -> s{_mDestinations = a}) . _Default . _Coerce

-- | The name of the multiplex.
mName :: Lens' Multiplex (Maybe Text)
mName = lens _mName (\ s a -> s{_mName = a})

-- | The unique id of the multiplex.
mId :: Lens' Multiplex (Maybe Text)
mId = lens _mId (\ s a -> s{_mId = a})

-- | Configuration for a multiplex event.
mMultiplexSettings :: Lens' Multiplex (Maybe MultiplexSettings)
mMultiplexSettings = lens _mMultiplexSettings (\ s a -> s{_mMultiplexSettings = a})

-- | A collection of key-value pairs.
mTags :: Lens' Multiplex (HashMap Text Text)
mTags = lens _mTags (\ s a -> s{_mTags = a}) . _Default . _Map

instance FromJSON Multiplex where
        parseJSON
          = withObject "Multiplex"
              (\ x ->
                 Multiplex' <$>
                   (x .:? "state") <*> (x .:? "arn") <*>
                     (x .:? "pipelinesRunningCount")
                     <*> (x .:? "availabilityZones" .!= mempty)
                     <*> (x .:? "programCount")
                     <*> (x .:? "destinations" .!= mempty)
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "multiplexSettings")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable Multiplex where

instance NFData Multiplex where

-- | Multiplex Group Settings
--
-- /See:/ 'multiplexGroupSettings' smart constructor.
data MultiplexGroupSettings = MultiplexGroupSettings'
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'MultiplexGroupSettings' with the minimum fields required to make a request.
--
multiplexGroupSettings
    :: MultiplexGroupSettings
multiplexGroupSettings = MultiplexGroupSettings'

instance FromJSON MultiplexGroupSettings where
        parseJSON
          = withObject "MultiplexGroupSettings"
              (\ x -> pure MultiplexGroupSettings')

instance Hashable MultiplexGroupSettings where

instance NFData MultiplexGroupSettings where

instance ToJSON MultiplexGroupSettings where
        toJSON = const (Object mempty)

-- | Multiplex MediaConnect output destination settings.
--
-- /See:/ 'multiplexMediaConnectOutputDestinationSettings' smart constructor.
newtype MultiplexMediaConnectOutputDestinationSettings = MultiplexMediaConnectOutputDestinationSettings'{_mmcodsEntitlementARN
                                                                                                         ::
                                                                                                         Maybe
                                                                                                           Text}
                                                           deriving (Eq, Read,
                                                                     Show, Data,
                                                                     Typeable,
                                                                     Generic)

-- | Creates a value of 'MultiplexMediaConnectOutputDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mmcodsEntitlementARN' - The MediaConnect entitlement ARN available as a Flow source.
multiplexMediaConnectOutputDestinationSettings
    :: MultiplexMediaConnectOutputDestinationSettings
multiplexMediaConnectOutputDestinationSettings
  = MultiplexMediaConnectOutputDestinationSettings'{_mmcodsEntitlementARN
                                                      = Nothing}

-- | The MediaConnect entitlement ARN available as a Flow source.
mmcodsEntitlementARN :: Lens' MultiplexMediaConnectOutputDestinationSettings (Maybe Text)
mmcodsEntitlementARN = lens _mmcodsEntitlementARN (\ s a -> s{_mmcodsEntitlementARN = a})

instance FromJSON
           MultiplexMediaConnectOutputDestinationSettings
         where
        parseJSON
          = withObject
              "MultiplexMediaConnectOutputDestinationSettings"
              (\ x ->
                 MultiplexMediaConnectOutputDestinationSettings' <$>
                   (x .:? "entitlementArn"))

instance Hashable
           MultiplexMediaConnectOutputDestinationSettings
         where

instance NFData
           MultiplexMediaConnectOutputDestinationSettings
         where

-- | Multiplex output destination settings
--
-- /See:/ 'multiplexOutputDestination' smart constructor.
newtype MultiplexOutputDestination = MultiplexOutputDestination'{_modMediaConnectSettings
                                                                 ::
                                                                 Maybe
                                                                   MultiplexMediaConnectOutputDestinationSettings}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'MultiplexOutputDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'modMediaConnectSettings' - Multiplex MediaConnect output destination settings.
multiplexOutputDestination
    :: MultiplexOutputDestination
multiplexOutputDestination
  = MultiplexOutputDestination'{_modMediaConnectSettings
                                  = Nothing}

-- | Multiplex MediaConnect output destination settings.
modMediaConnectSettings :: Lens' MultiplexOutputDestination (Maybe MultiplexMediaConnectOutputDestinationSettings)
modMediaConnectSettings = lens _modMediaConnectSettings (\ s a -> s{_modMediaConnectSettings = a})

instance FromJSON MultiplexOutputDestination where
        parseJSON
          = withObject "MultiplexOutputDestination"
              (\ x ->
                 MultiplexOutputDestination' <$>
                   (x .:? "mediaConnectSettings"))

instance Hashable MultiplexOutputDestination where

instance NFData MultiplexOutputDestination where

-- | Multiplex Output Settings
--
-- /See:/ 'multiplexOutputSettings' smart constructor.
newtype MultiplexOutputSettings = MultiplexOutputSettings'{_mosDestination
                                                           :: OutputLocationRef}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'MultiplexOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mosDestination' - Destination is a Multiplex.
multiplexOutputSettings
    :: OutputLocationRef -- ^ 'mosDestination'
    -> MultiplexOutputSettings
multiplexOutputSettings pDestination_
  = MultiplexOutputSettings'{_mosDestination =
                               pDestination_}

-- | Destination is a Multiplex.
mosDestination :: Lens' MultiplexOutputSettings OutputLocationRef
mosDestination = lens _mosDestination (\ s a -> s{_mosDestination = a})

instance FromJSON MultiplexOutputSettings where
        parseJSON
          = withObject "MultiplexOutputSettings"
              (\ x ->
                 MultiplexOutputSettings' <$> (x .: "destination"))

instance Hashable MultiplexOutputSettings where

instance NFData MultiplexOutputSettings where

instance ToJSON MultiplexOutputSettings where
        toJSON MultiplexOutputSettings'{..}
          = object
              (catMaybes [Just ("destination" .= _mosDestination)])

-- | The multiplex program object.
--
-- /See:/ 'multiplexProgram' smart constructor.
data MultiplexProgram = MultiplexProgram'{_mpPacketIdentifiersMap
                                          ::
                                          !(Maybe
                                              MultiplexProgramPacketIdentifiersMap),
                                          _mpProgramName :: !(Maybe Text),
                                          _mpChannelId :: !(Maybe Text),
                                          _mpMultiplexProgramSettings ::
                                          !(Maybe MultiplexProgramSettings)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexProgram' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpPacketIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- * 'mpProgramName' - The name of the multiplex program.
--
-- * 'mpChannelId' - The MediaLive channel associated with the program.
--
-- * 'mpMultiplexProgramSettings' - The settings for this multiplex program.
multiplexProgram
    :: MultiplexProgram
multiplexProgram
  = MultiplexProgram'{_mpPacketIdentifiersMap =
                        Nothing,
                      _mpProgramName = Nothing, _mpChannelId = Nothing,
                      _mpMultiplexProgramSettings = Nothing}

-- | The packet identifier map for this multiplex program.
mpPacketIdentifiersMap :: Lens' MultiplexProgram (Maybe MultiplexProgramPacketIdentifiersMap)
mpPacketIdentifiersMap = lens _mpPacketIdentifiersMap (\ s a -> s{_mpPacketIdentifiersMap = a})

-- | The name of the multiplex program.
mpProgramName :: Lens' MultiplexProgram (Maybe Text)
mpProgramName = lens _mpProgramName (\ s a -> s{_mpProgramName = a})

-- | The MediaLive channel associated with the program.
mpChannelId :: Lens' MultiplexProgram (Maybe Text)
mpChannelId = lens _mpChannelId (\ s a -> s{_mpChannelId = a})

-- | The settings for this multiplex program.
mpMultiplexProgramSettings :: Lens' MultiplexProgram (Maybe MultiplexProgramSettings)
mpMultiplexProgramSettings = lens _mpMultiplexProgramSettings (\ s a -> s{_mpMultiplexProgramSettings = a})

instance FromJSON MultiplexProgram where
        parseJSON
          = withObject "MultiplexProgram"
              (\ x ->
                 MultiplexProgram' <$>
                   (x .:? "packetIdentifiersMap") <*>
                     (x .:? "programName")
                     <*> (x .:? "channelId")
                     <*> (x .:? "multiplexProgramSettings"))

instance Hashable MultiplexProgram where

instance NFData MultiplexProgram where

-- | Multiplex Program Input Destination Settings for outputting a Channel to a Multiplex
--
-- /See:/ 'multiplexProgramChannelDestinationSettings' smart constructor.
data MultiplexProgramChannelDestinationSettings = MultiplexProgramChannelDestinationSettings'{_mpcdsMultiplexId
                                                                                              ::
                                                                                              !(Maybe
                                                                                                  Text),
                                                                                              _mpcdsProgramName
                                                                                              ::
                                                                                              !(Maybe
                                                                                                  Text)}
                                                    deriving (Eq, Read, Show,
                                                              Data, Typeable,
                                                              Generic)

-- | Creates a value of 'MultiplexProgramChannelDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpcdsMultiplexId' - The ID of the Multiplex that the encoder is providing output to. You do not need to specify the individual inputs to the Multiplex; MediaLive will handle the connection of the two MediaLive pipelines to the two Multiplex instances. The Multiplex must be in the same region as the Channel.
--
-- * 'mpcdsProgramName' - The program name of the Multiplex program that the encoder is providing output to.
multiplexProgramChannelDestinationSettings
    :: MultiplexProgramChannelDestinationSettings
multiplexProgramChannelDestinationSettings
  = MultiplexProgramChannelDestinationSettings'{_mpcdsMultiplexId
                                                  = Nothing,
                                                _mpcdsProgramName = Nothing}

-- | The ID of the Multiplex that the encoder is providing output to. You do not need to specify the individual inputs to the Multiplex; MediaLive will handle the connection of the two MediaLive pipelines to the two Multiplex instances. The Multiplex must be in the same region as the Channel.
mpcdsMultiplexId :: Lens' MultiplexProgramChannelDestinationSettings (Maybe Text)
mpcdsMultiplexId = lens _mpcdsMultiplexId (\ s a -> s{_mpcdsMultiplexId = a})

-- | The program name of the Multiplex program that the encoder is providing output to.
mpcdsProgramName :: Lens' MultiplexProgramChannelDestinationSettings (Maybe Text)
mpcdsProgramName = lens _mpcdsProgramName (\ s a -> s{_mpcdsProgramName = a})

instance FromJSON
           MultiplexProgramChannelDestinationSettings
         where
        parseJSON
          = withObject
              "MultiplexProgramChannelDestinationSettings"
              (\ x ->
                 MultiplexProgramChannelDestinationSettings' <$>
                   (x .:? "multiplexId") <*> (x .:? "programName"))

instance Hashable
           MultiplexProgramChannelDestinationSettings
         where

instance NFData
           MultiplexProgramChannelDestinationSettings
         where

instance ToJSON
           MultiplexProgramChannelDestinationSettings
         where
        toJSON
          MultiplexProgramChannelDestinationSettings'{..}
          = object
              (catMaybes
                 [("multiplexId" .=) <$> _mpcdsMultiplexId,
                  ("programName" .=) <$> _mpcdsProgramName])

-- | Packet identifiers map for a given Multiplex program.
--
-- /See:/ 'multiplexProgramPacketIdentifiersMap' smart constructor.
data MultiplexProgramPacketIdentifiersMap = MultiplexProgramPacketIdentifiersMap'{_mppimPmtPid
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Int),
                                                                                  _mppimEtvSignalPid
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Int),
                                                                                  _mppimVideoPid
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Int),
                                                                                  _mppimScte35Pid
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Int),
                                                                                  _mppimPrivateMetadataPid
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Int),
                                                                                  _mppimTimedMetadataPid
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Int),
                                                                                  _mppimPcrPid
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Int),
                                                                                  _mppimKlvDataPids
                                                                                  ::
                                                                                  !(Maybe
                                                                                      [Int]),
                                                                                  _mppimDvbSubPids
                                                                                  ::
                                                                                  !(Maybe
                                                                                      [Int]),
                                                                                  _mppimScte27Pids
                                                                                  ::
                                                                                  !(Maybe
                                                                                      [Int]),
                                                                                  _mppimEtvPlatformPid
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Int),
                                                                                  _mppimAudioPids
                                                                                  ::
                                                                                  !(Maybe
                                                                                      [Int]),
                                                                                  _mppimDvbTeletextPid
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Int)}
                                              deriving (Eq, Read, Show, Data,
                                                        Typeable, Generic)

-- | Creates a value of 'MultiplexProgramPacketIdentifiersMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mppimPmtPid' - Undocumented member.
--
-- * 'mppimEtvSignalPid' - Undocumented member.
--
-- * 'mppimVideoPid' - Undocumented member.
--
-- * 'mppimScte35Pid' - Undocumented member.
--
-- * 'mppimPrivateMetadataPid' - Undocumented member.
--
-- * 'mppimTimedMetadataPid' - Undocumented member.
--
-- * 'mppimPcrPid' - Undocumented member.
--
-- * 'mppimKlvDataPids' - Undocumented member.
--
-- * 'mppimDvbSubPids' - Undocumented member.
--
-- * 'mppimScte27Pids' - Undocumented member.
--
-- * 'mppimEtvPlatformPid' - Undocumented member.
--
-- * 'mppimAudioPids' - Undocumented member.
--
-- * 'mppimDvbTeletextPid' - Undocumented member.
multiplexProgramPacketIdentifiersMap
    :: MultiplexProgramPacketIdentifiersMap
multiplexProgramPacketIdentifiersMap
  = MultiplexProgramPacketIdentifiersMap'{_mppimPmtPid
                                            = Nothing,
                                          _mppimEtvSignalPid = Nothing,
                                          _mppimVideoPid = Nothing,
                                          _mppimScte35Pid = Nothing,
                                          _mppimPrivateMetadataPid = Nothing,
                                          _mppimTimedMetadataPid = Nothing,
                                          _mppimPcrPid = Nothing,
                                          _mppimKlvDataPids = Nothing,
                                          _mppimDvbSubPids = Nothing,
                                          _mppimScte27Pids = Nothing,
                                          _mppimEtvPlatformPid = Nothing,
                                          _mppimAudioPids = Nothing,
                                          _mppimDvbTeletextPid = Nothing}

-- | Undocumented member.
mppimPmtPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimPmtPid = lens _mppimPmtPid (\ s a -> s{_mppimPmtPid = a})

-- | Undocumented member.
mppimEtvSignalPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimEtvSignalPid = lens _mppimEtvSignalPid (\ s a -> s{_mppimEtvSignalPid = a})

-- | Undocumented member.
mppimVideoPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimVideoPid = lens _mppimVideoPid (\ s a -> s{_mppimVideoPid = a})

-- | Undocumented member.
mppimScte35Pid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimScte35Pid = lens _mppimScte35Pid (\ s a -> s{_mppimScte35Pid = a})

-- | Undocumented member.
mppimPrivateMetadataPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimPrivateMetadataPid = lens _mppimPrivateMetadataPid (\ s a -> s{_mppimPrivateMetadataPid = a})

-- | Undocumented member.
mppimTimedMetadataPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimTimedMetadataPid = lens _mppimTimedMetadataPid (\ s a -> s{_mppimTimedMetadataPid = a})

-- | Undocumented member.
mppimPcrPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimPcrPid = lens _mppimPcrPid (\ s a -> s{_mppimPcrPid = a})

-- | Undocumented member.
mppimKlvDataPids :: Lens' MultiplexProgramPacketIdentifiersMap [Int]
mppimKlvDataPids = lens _mppimKlvDataPids (\ s a -> s{_mppimKlvDataPids = a}) . _Default . _Coerce

-- | Undocumented member.
mppimDvbSubPids :: Lens' MultiplexProgramPacketIdentifiersMap [Int]
mppimDvbSubPids = lens _mppimDvbSubPids (\ s a -> s{_mppimDvbSubPids = a}) . _Default . _Coerce

-- | Undocumented member.
mppimScte27Pids :: Lens' MultiplexProgramPacketIdentifiersMap [Int]
mppimScte27Pids = lens _mppimScte27Pids (\ s a -> s{_mppimScte27Pids = a}) . _Default . _Coerce

-- | Undocumented member.
mppimEtvPlatformPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimEtvPlatformPid = lens _mppimEtvPlatformPid (\ s a -> s{_mppimEtvPlatformPid = a})

-- | Undocumented member.
mppimAudioPids :: Lens' MultiplexProgramPacketIdentifiersMap [Int]
mppimAudioPids = lens _mppimAudioPids (\ s a -> s{_mppimAudioPids = a}) . _Default . _Coerce

-- | Undocumented member.
mppimDvbTeletextPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimDvbTeletextPid = lens _mppimDvbTeletextPid (\ s a -> s{_mppimDvbTeletextPid = a})

instance FromJSON
           MultiplexProgramPacketIdentifiersMap
         where
        parseJSON
          = withObject "MultiplexProgramPacketIdentifiersMap"
              (\ x ->
                 MultiplexProgramPacketIdentifiersMap' <$>
                   (x .:? "pmtPid") <*> (x .:? "etvSignalPid") <*>
                     (x .:? "videoPid")
                     <*> (x .:? "scte35Pid")
                     <*> (x .:? "privateMetadataPid")
                     <*> (x .:? "timedMetadataPid")
                     <*> (x .:? "pcrPid")
                     <*> (x .:? "klvDataPids" .!= mempty)
                     <*> (x .:? "dvbSubPids" .!= mempty)
                     <*> (x .:? "scte27Pids" .!= mempty)
                     <*> (x .:? "etvPlatformPid")
                     <*> (x .:? "audioPids" .!= mempty)
                     <*> (x .:? "dvbTeletextPid"))

instance Hashable
           MultiplexProgramPacketIdentifiersMap
         where

instance NFData MultiplexProgramPacketIdentifiersMap
         where

-- | Transport stream service descriptor configuration for the Multiplex program.
--
-- /See:/ 'multiplexProgramServiceDescriptor' smart constructor.
data MultiplexProgramServiceDescriptor = MultiplexProgramServiceDescriptor'{_mpsdProviderName
                                                                            ::
                                                                            !Text,
                                                                            _mpsdServiceName
                                                                            ::
                                                                            !Text}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'MultiplexProgramServiceDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsdProviderName' - Name of the provider.
--
-- * 'mpsdServiceName' - Name of the service.
multiplexProgramServiceDescriptor
    :: Text -- ^ 'mpsdProviderName'
    -> Text -- ^ 'mpsdServiceName'
    -> MultiplexProgramServiceDescriptor
multiplexProgramServiceDescriptor pProviderName_
  pServiceName_
  = MultiplexProgramServiceDescriptor'{_mpsdProviderName
                                         = pProviderName_,
                                       _mpsdServiceName = pServiceName_}

-- | Name of the provider.
mpsdProviderName :: Lens' MultiplexProgramServiceDescriptor Text
mpsdProviderName = lens _mpsdProviderName (\ s a -> s{_mpsdProviderName = a})

-- | Name of the service.
mpsdServiceName :: Lens' MultiplexProgramServiceDescriptor Text
mpsdServiceName = lens _mpsdServiceName (\ s a -> s{_mpsdServiceName = a})

instance FromJSON MultiplexProgramServiceDescriptor
         where
        parseJSON
          = withObject "MultiplexProgramServiceDescriptor"
              (\ x ->
                 MultiplexProgramServiceDescriptor' <$>
                   (x .: "providerName") <*> (x .: "serviceName"))

instance Hashable MultiplexProgramServiceDescriptor
         where

instance NFData MultiplexProgramServiceDescriptor
         where

instance ToJSON MultiplexProgramServiceDescriptor
         where
        toJSON MultiplexProgramServiceDescriptor'{..}
          = object
              (catMaybes
                 [Just ("providerName" .= _mpsdProviderName),
                  Just ("serviceName" .= _mpsdServiceName)])

-- | Multiplex Program settings configuration.
--
-- /See:/ 'multiplexProgramSettings' smart constructor.
data MultiplexProgramSettings = MultiplexProgramSettings'{_mpsPreferredChannelPipeline
                                                          ::
                                                          !(Maybe
                                                              PreferredChannelPipeline),
                                                          _mpsVideoSettings ::
                                                          !(Maybe
                                                              MultiplexVideoSettings),
                                                          _mpsServiceDescriptor
                                                          ::
                                                          !(Maybe
                                                              MultiplexProgramServiceDescriptor),
                                                          _mpsProgramNumber ::
                                                          !Nat}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'MultiplexProgramSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsPreferredChannelPipeline' - Indicates which pipeline is preferred by the multiplex for program ingest.
--
-- * 'mpsVideoSettings' - Program video settings configuration.
--
-- * 'mpsServiceDescriptor' - Transport stream service descriptor configuration for the Multiplex program.
--
-- * 'mpsProgramNumber' - Unique program number.
multiplexProgramSettings
    :: Natural -- ^ 'mpsProgramNumber'
    -> MultiplexProgramSettings
multiplexProgramSettings pProgramNumber_
  = MultiplexProgramSettings'{_mpsPreferredChannelPipeline
                                = Nothing,
                              _mpsVideoSettings = Nothing,
                              _mpsServiceDescriptor = Nothing,
                              _mpsProgramNumber = _Nat # pProgramNumber_}

-- | Indicates which pipeline is preferred by the multiplex for program ingest.
mpsPreferredChannelPipeline :: Lens' MultiplexProgramSettings (Maybe PreferredChannelPipeline)
mpsPreferredChannelPipeline = lens _mpsPreferredChannelPipeline (\ s a -> s{_mpsPreferredChannelPipeline = a})

-- | Program video settings configuration.
mpsVideoSettings :: Lens' MultiplexProgramSettings (Maybe MultiplexVideoSettings)
mpsVideoSettings = lens _mpsVideoSettings (\ s a -> s{_mpsVideoSettings = a})

-- | Transport stream service descriptor configuration for the Multiplex program.
mpsServiceDescriptor :: Lens' MultiplexProgramSettings (Maybe MultiplexProgramServiceDescriptor)
mpsServiceDescriptor = lens _mpsServiceDescriptor (\ s a -> s{_mpsServiceDescriptor = a})

-- | Unique program number.
mpsProgramNumber :: Lens' MultiplexProgramSettings Natural
mpsProgramNumber = lens _mpsProgramNumber (\ s a -> s{_mpsProgramNumber = a}) . _Nat

instance FromJSON MultiplexProgramSettings where
        parseJSON
          = withObject "MultiplexProgramSettings"
              (\ x ->
                 MultiplexProgramSettings' <$>
                   (x .:? "preferredChannelPipeline") <*>
                     (x .:? "videoSettings")
                     <*> (x .:? "serviceDescriptor")
                     <*> (x .: "programNumber"))

instance Hashable MultiplexProgramSettings where

instance NFData MultiplexProgramSettings where

instance ToJSON MultiplexProgramSettings where
        toJSON MultiplexProgramSettings'{..}
          = object
              (catMaybes
                 [("preferredChannelPipeline" .=) <$>
                    _mpsPreferredChannelPipeline,
                  ("videoSettings" .=) <$> _mpsVideoSettings,
                  ("serviceDescriptor" .=) <$> _mpsServiceDescriptor,
                  Just ("programNumber" .= _mpsProgramNumber)])

-- | Placeholder documentation for MultiplexProgramSummary
--
-- /See:/ 'multiplexProgramSummary' smart constructor.
data MultiplexProgramSummary = MultiplexProgramSummary'{_mpsProgramName
                                                        :: !(Maybe Text),
                                                        _mpsChannelId ::
                                                        !(Maybe Text)}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'MultiplexProgramSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsProgramName' - The name of the multiplex program.
--
-- * 'mpsChannelId' - The MediaLive Channel associated with the program.
multiplexProgramSummary
    :: MultiplexProgramSummary
multiplexProgramSummary
  = MultiplexProgramSummary'{_mpsProgramName = Nothing,
                             _mpsChannelId = Nothing}

-- | The name of the multiplex program.
mpsProgramName :: Lens' MultiplexProgramSummary (Maybe Text)
mpsProgramName = lens _mpsProgramName (\ s a -> s{_mpsProgramName = a})

-- | The MediaLive Channel associated with the program.
mpsChannelId :: Lens' MultiplexProgramSummary (Maybe Text)
mpsChannelId = lens _mpsChannelId (\ s a -> s{_mpsChannelId = a})

instance FromJSON MultiplexProgramSummary where
        parseJSON
          = withObject "MultiplexProgramSummary"
              (\ x ->
                 MultiplexProgramSummary' <$>
                   (x .:? "programName") <*> (x .:? "channelId"))

instance Hashable MultiplexProgramSummary where

instance NFData MultiplexProgramSummary where

-- | Contains configuration for a Multiplex event
--
-- /See:/ 'multiplexSettings' smart constructor.
data MultiplexSettings = MultiplexSettings'{_mssMaximumVideoBufferDelayMilliseconds
                                            :: !(Maybe Nat),
                                            _mssTransportStreamReservedBitrate
                                            :: !(Maybe Nat),
                                            _mssTransportStreamBitrate :: !Nat,
                                            _mssTransportStreamId :: !Nat}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mssMaximumVideoBufferDelayMilliseconds' - Maximum video buffer delay in milliseconds.
--
-- * 'mssTransportStreamReservedBitrate' - Transport stream reserved bit rate.
--
-- * 'mssTransportStreamBitrate' - Transport stream bit rate.
--
-- * 'mssTransportStreamId' - Transport stream ID.
multiplexSettings
    :: Natural -- ^ 'mssTransportStreamBitrate'
    -> Natural -- ^ 'mssTransportStreamId'
    -> MultiplexSettings
multiplexSettings pTransportStreamBitrate_
  pTransportStreamId_
  = MultiplexSettings'{_mssMaximumVideoBufferDelayMilliseconds
                         = Nothing,
                       _mssTransportStreamReservedBitrate = Nothing,
                       _mssTransportStreamBitrate =
                         _Nat # pTransportStreamBitrate_,
                       _mssTransportStreamId = _Nat # pTransportStreamId_}

-- | Maximum video buffer delay in milliseconds.
mssMaximumVideoBufferDelayMilliseconds :: Lens' MultiplexSettings (Maybe Natural)
mssMaximumVideoBufferDelayMilliseconds = lens _mssMaximumVideoBufferDelayMilliseconds (\ s a -> s{_mssMaximumVideoBufferDelayMilliseconds = a}) . mapping _Nat

-- | Transport stream reserved bit rate.
mssTransportStreamReservedBitrate :: Lens' MultiplexSettings (Maybe Natural)
mssTransportStreamReservedBitrate = lens _mssTransportStreamReservedBitrate (\ s a -> s{_mssTransportStreamReservedBitrate = a}) . mapping _Nat

-- | Transport stream bit rate.
mssTransportStreamBitrate :: Lens' MultiplexSettings Natural
mssTransportStreamBitrate = lens _mssTransportStreamBitrate (\ s a -> s{_mssTransportStreamBitrate = a}) . _Nat

-- | Transport stream ID.
mssTransportStreamId :: Lens' MultiplexSettings Natural
mssTransportStreamId = lens _mssTransportStreamId (\ s a -> s{_mssTransportStreamId = a}) . _Nat

instance FromJSON MultiplexSettings where
        parseJSON
          = withObject "MultiplexSettings"
              (\ x ->
                 MultiplexSettings' <$>
                   (x .:? "maximumVideoBufferDelayMilliseconds") <*>
                     (x .:? "transportStreamReservedBitrate")
                     <*> (x .: "transportStreamBitrate")
                     <*> (x .: "transportStreamId"))

instance Hashable MultiplexSettings where

instance NFData MultiplexSettings where

instance ToJSON MultiplexSettings where
        toJSON MultiplexSettings'{..}
          = object
              (catMaybes
                 [("maximumVideoBufferDelayMilliseconds" .=) <$>
                    _mssMaximumVideoBufferDelayMilliseconds,
                  ("transportStreamReservedBitrate" .=) <$>
                    _mssTransportStreamReservedBitrate,
                  Just
                    ("transportStreamBitrate" .=
                       _mssTransportStreamBitrate),
                  Just ("transportStreamId" .= _mssTransportStreamId)])

-- | Contains summary configuration for a Multiplex event.
--
-- /See:/ 'multiplexSettingsSummary' smart constructor.
newtype MultiplexSettingsSummary = MultiplexSettingsSummary'{_mTransportStreamBitrate
                                                             :: Maybe Nat}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'MultiplexSettingsSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mTransportStreamBitrate' - Transport stream bit rate.
multiplexSettingsSummary
    :: MultiplexSettingsSummary
multiplexSettingsSummary
  = MultiplexSettingsSummary'{_mTransportStreamBitrate
                                = Nothing}

-- | Transport stream bit rate.
mTransportStreamBitrate :: Lens' MultiplexSettingsSummary (Maybe Natural)
mTransportStreamBitrate = lens _mTransportStreamBitrate (\ s a -> s{_mTransportStreamBitrate = a}) . mapping _Nat

instance FromJSON MultiplexSettingsSummary where
        parseJSON
          = withObject "MultiplexSettingsSummary"
              (\ x ->
                 MultiplexSettingsSummary' <$>
                   (x .:? "transportStreamBitrate"))

instance Hashable MultiplexSettingsSummary where

instance NFData MultiplexSettingsSummary where

-- | Statmux rate control settings
--
-- /See:/ 'multiplexStatmuxVideoSettings' smart constructor.
data MultiplexStatmuxVideoSettings = MultiplexStatmuxVideoSettings'{_msvsMinimumBitrate
                                                                    ::
                                                                    !(Maybe
                                                                        Nat),
                                                                    _msvsMaximumBitrate
                                                                    ::
                                                                    !(Maybe
                                                                        Nat)}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'MultiplexStatmuxVideoSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msvsMinimumBitrate' - Minimum statmux bitrate.
--
-- * 'msvsMaximumBitrate' - Maximum statmux bitrate.
multiplexStatmuxVideoSettings
    :: MultiplexStatmuxVideoSettings
multiplexStatmuxVideoSettings
  = MultiplexStatmuxVideoSettings'{_msvsMinimumBitrate
                                     = Nothing,
                                   _msvsMaximumBitrate = Nothing}

-- | Minimum statmux bitrate.
msvsMinimumBitrate :: Lens' MultiplexStatmuxVideoSettings (Maybe Natural)
msvsMinimumBitrate = lens _msvsMinimumBitrate (\ s a -> s{_msvsMinimumBitrate = a}) . mapping _Nat

-- | Maximum statmux bitrate.
msvsMaximumBitrate :: Lens' MultiplexStatmuxVideoSettings (Maybe Natural)
msvsMaximumBitrate = lens _msvsMaximumBitrate (\ s a -> s{_msvsMaximumBitrate = a}) . mapping _Nat

instance FromJSON MultiplexStatmuxVideoSettings where
        parseJSON
          = withObject "MultiplexStatmuxVideoSettings"
              (\ x ->
                 MultiplexStatmuxVideoSettings' <$>
                   (x .:? "minimumBitrate") <*>
                     (x .:? "maximumBitrate"))

instance Hashable MultiplexStatmuxVideoSettings where

instance NFData MultiplexStatmuxVideoSettings where

instance ToJSON MultiplexStatmuxVideoSettings where
        toJSON MultiplexStatmuxVideoSettings'{..}
          = object
              (catMaybes
                 [("minimumBitrate" .=) <$> _msvsMinimumBitrate,
                  ("maximumBitrate" .=) <$> _msvsMaximumBitrate])

-- | Placeholder documentation for MultiplexSummary
--
-- /See:/ 'multiplexSummary' smart constructor.
data MultiplexSummary = MultiplexSummary'{_msState ::
                                          !(Maybe MultiplexState),
                                          _msARN :: !(Maybe Text),
                                          _msPipelinesRunningCount ::
                                          !(Maybe Int),
                                          _msAvailabilityZones ::
                                          !(Maybe [Text]),
                                          _msProgramCount :: !(Maybe Int),
                                          _msName :: !(Maybe Text),
                                          _msId :: !(Maybe Text),
                                          _msMultiplexSettings ::
                                          !(Maybe MultiplexSettingsSummary),
                                          _msTags :: !(Maybe (Map Text Text))}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msState' - The current state of the multiplex.
--
-- * 'msARN' - The unique arn of the multiplex.
--
-- * 'msPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'msAvailabilityZones' - A list of availability zones for the multiplex.
--
-- * 'msProgramCount' - The number of programs in the multiplex.
--
-- * 'msName' - The name of the multiplex.
--
-- * 'msId' - The unique id of the multiplex.
--
-- * 'msMultiplexSettings' - Configuration for a multiplex event.
--
-- * 'msTags' - A collection of key-value pairs.
multiplexSummary
    :: MultiplexSummary
multiplexSummary
  = MultiplexSummary'{_msState = Nothing,
                      _msARN = Nothing, _msPipelinesRunningCount = Nothing,
                      _msAvailabilityZones = Nothing,
                      _msProgramCount = Nothing, _msName = Nothing,
                      _msId = Nothing, _msMultiplexSettings = Nothing,
                      _msTags = Nothing}

-- | The current state of the multiplex.
msState :: Lens' MultiplexSummary (Maybe MultiplexState)
msState = lens _msState (\ s a -> s{_msState = a})

-- | The unique arn of the multiplex.
msARN :: Lens' MultiplexSummary (Maybe Text)
msARN = lens _msARN (\ s a -> s{_msARN = a})

-- | The number of currently healthy pipelines.
msPipelinesRunningCount :: Lens' MultiplexSummary (Maybe Int)
msPipelinesRunningCount = lens _msPipelinesRunningCount (\ s a -> s{_msPipelinesRunningCount = a})

-- | A list of availability zones for the multiplex.
msAvailabilityZones :: Lens' MultiplexSummary [Text]
msAvailabilityZones = lens _msAvailabilityZones (\ s a -> s{_msAvailabilityZones = a}) . _Default . _Coerce

-- | The number of programs in the multiplex.
msProgramCount :: Lens' MultiplexSummary (Maybe Int)
msProgramCount = lens _msProgramCount (\ s a -> s{_msProgramCount = a})

-- | The name of the multiplex.
msName :: Lens' MultiplexSummary (Maybe Text)
msName = lens _msName (\ s a -> s{_msName = a})

-- | The unique id of the multiplex.
msId :: Lens' MultiplexSummary (Maybe Text)
msId = lens _msId (\ s a -> s{_msId = a})

-- | Configuration for a multiplex event.
msMultiplexSettings :: Lens' MultiplexSummary (Maybe MultiplexSettingsSummary)
msMultiplexSettings = lens _msMultiplexSettings (\ s a -> s{_msMultiplexSettings = a})

-- | A collection of key-value pairs.
msTags :: Lens' MultiplexSummary (HashMap Text Text)
msTags = lens _msTags (\ s a -> s{_msTags = a}) . _Default . _Map

instance FromJSON MultiplexSummary where
        parseJSON
          = withObject "MultiplexSummary"
              (\ x ->
                 MultiplexSummary' <$>
                   (x .:? "state") <*> (x .:? "arn") <*>
                     (x .:? "pipelinesRunningCount")
                     <*> (x .:? "availabilityZones" .!= mempty)
                     <*> (x .:? "programCount")
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "multiplexSettings")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable MultiplexSummary where

instance NFData MultiplexSummary where

-- | The video configuration for each program in a multiplex.
--
-- /See:/ 'multiplexVideoSettings' smart constructor.
data MultiplexVideoSettings = MultiplexVideoSettings'{_mvsStatmuxSettings
                                                      ::
                                                      !(Maybe
                                                          MultiplexStatmuxVideoSettings),
                                                      _mvsConstantBitrate ::
                                                      !(Maybe Nat)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'MultiplexVideoSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvsStatmuxSettings' - Statmux rate control settings. When this field is defined, ConstantBitrate must be undefined.
--
-- * 'mvsConstantBitrate' - The constant bitrate configuration for the video encode. When this field is defined, StatmuxSettings must be undefined.
multiplexVideoSettings
    :: MultiplexVideoSettings
multiplexVideoSettings
  = MultiplexVideoSettings'{_mvsStatmuxSettings =
                              Nothing,
                            _mvsConstantBitrate = Nothing}

-- | Statmux rate control settings. When this field is defined, ConstantBitrate must be undefined.
mvsStatmuxSettings :: Lens' MultiplexVideoSettings (Maybe MultiplexStatmuxVideoSettings)
mvsStatmuxSettings = lens _mvsStatmuxSettings (\ s a -> s{_mvsStatmuxSettings = a})

-- | The constant bitrate configuration for the video encode. When this field is defined, StatmuxSettings must be undefined.
mvsConstantBitrate :: Lens' MultiplexVideoSettings (Maybe Natural)
mvsConstantBitrate = lens _mvsConstantBitrate (\ s a -> s{_mvsConstantBitrate = a}) . mapping _Nat

instance FromJSON MultiplexVideoSettings where
        parseJSON
          = withObject "MultiplexVideoSettings"
              (\ x ->
                 MultiplexVideoSettings' <$>
                   (x .:? "statmuxSettings") <*>
                     (x .:? "constantBitrate"))

instance Hashable MultiplexVideoSettings where

instance NFData MultiplexVideoSettings where

instance ToJSON MultiplexVideoSettings where
        toJSON MultiplexVideoSettings'{..}
          = object
              (catMaybes
                 [("statmuxSettings" .=) <$> _mvsStatmuxSettings,
                  ("constantBitrate" .=) <$> _mvsConstantBitrate])

-- | Network source to transcode. Must be accessible to the Elemental Live node that is running the live event through a network connection.
--
-- /See:/ 'networkInputSettings' smart constructor.
data NetworkInputSettings = NetworkInputSettings'{_nisHlsInputSettings
                                                  :: !(Maybe HlsInputSettings),
                                                  _nisServerValidation ::
                                                  !(Maybe
                                                      NetworkInputServerValidation)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nisHlsInputSettings' - Specifies HLS input settings when the uri is for a HLS manifest.
--
-- * 'nisServerValidation' - Check HTTPS server certificates. When set to checkCryptographyOnly, cryptography in the certificate will be checked, but not the server's name. Certain subdomains (notably S3 buckets that use dots in the bucket name) do not strictly match the corresponding certificate's wildcard pattern and would otherwise cause the event to error. This setting is ignored for protocols that do not use https.
networkInputSettings
    :: NetworkInputSettings
networkInputSettings
  = NetworkInputSettings'{_nisHlsInputSettings =
                            Nothing,
                          _nisServerValidation = Nothing}

-- | Specifies HLS input settings when the uri is for a HLS manifest.
nisHlsInputSettings :: Lens' NetworkInputSettings (Maybe HlsInputSettings)
nisHlsInputSettings = lens _nisHlsInputSettings (\ s a -> s{_nisHlsInputSettings = a})

-- | Check HTTPS server certificates. When set to checkCryptographyOnly, cryptography in the certificate will be checked, but not the server's name. Certain subdomains (notably S3 buckets that use dots in the bucket name) do not strictly match the corresponding certificate's wildcard pattern and would otherwise cause the event to error. This setting is ignored for protocols that do not use https.
nisServerValidation :: Lens' NetworkInputSettings (Maybe NetworkInputServerValidation)
nisServerValidation = lens _nisServerValidation (\ s a -> s{_nisServerValidation = a})

instance FromJSON NetworkInputSettings where
        parseJSON
          = withObject "NetworkInputSettings"
              (\ x ->
                 NetworkInputSettings' <$>
                   (x .:? "hlsInputSettings") <*>
                     (x .:? "serverValidation"))

instance Hashable NetworkInputSettings where

instance NFData NetworkInputSettings where

instance ToJSON NetworkInputSettings where
        toJSON NetworkInputSettings'{..}
          = object
              (catMaybes
                 [("hlsInputSettings" .=) <$> _nisHlsInputSettings,
                  ("serverValidation" .=) <$> _nisServerValidation])

-- | Nielsen Configuration
--
-- /See:/ 'nielsenConfiguration' smart constructor.
data NielsenConfiguration = NielsenConfiguration'{_ncDistributorId
                                                  :: !(Maybe Text),
                                                  _ncNielsenPcmToId3Tagging ::
                                                  !(Maybe
                                                      NielsenPcmToId3TaggingState)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NielsenConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncDistributorId' - Enter the Distributor ID assigned to your organization by Nielsen.
--
-- * 'ncNielsenPcmToId3Tagging' - Enables Nielsen PCM to ID3 tagging
nielsenConfiguration
    :: NielsenConfiguration
nielsenConfiguration
  = NielsenConfiguration'{_ncDistributorId = Nothing,
                          _ncNielsenPcmToId3Tagging = Nothing}

-- | Enter the Distributor ID assigned to your organization by Nielsen.
ncDistributorId :: Lens' NielsenConfiguration (Maybe Text)
ncDistributorId = lens _ncDistributorId (\ s a -> s{_ncDistributorId = a})

-- | Enables Nielsen PCM to ID3 tagging
ncNielsenPcmToId3Tagging :: Lens' NielsenConfiguration (Maybe NielsenPcmToId3TaggingState)
ncNielsenPcmToId3Tagging = lens _ncNielsenPcmToId3Tagging (\ s a -> s{_ncNielsenPcmToId3Tagging = a})

instance FromJSON NielsenConfiguration where
        parseJSON
          = withObject "NielsenConfiguration"
              (\ x ->
                 NielsenConfiguration' <$>
                   (x .:? "distributorId") <*>
                     (x .:? "nielsenPcmToId3Tagging"))

instance Hashable NielsenConfiguration where

instance NFData NielsenConfiguration where

instance ToJSON NielsenConfiguration where
        toJSON NielsenConfiguration'{..}
          = object
              (catMaybes
                 [("distributorId" .=) <$> _ncDistributorId,
                  ("nielsenPcmToId3Tagging" .=) <$>
                    _ncNielsenPcmToId3Tagging])

-- | Reserved resources available for purchase
--
-- /See:/ 'offering' smart constructor.
data Offering = Offering'{_oResourceSpecification ::
                          !(Maybe ReservationResourceSpecification),
                          _oCurrencyCode :: !(Maybe Text),
                          _oARN :: !(Maybe Text),
                          _oOfferingId :: !(Maybe Text),
                          _oRegion :: !(Maybe Text),
                          _oOfferingType :: !(Maybe OfferingType),
                          _oUsagePrice :: !(Maybe Double),
                          _oFixedPrice :: !(Maybe Double),
                          _oDurationUnits :: !(Maybe OfferingDurationUnits),
                          _oOfferingDescription :: !(Maybe Text),
                          _oDuration :: !(Maybe Int)}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Offering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oResourceSpecification' - Resource configuration details
--
-- * 'oCurrencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- * 'oARN' - Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
--
-- * 'oOfferingId' - Unique offering ID, e.g. '87654321'
--
-- * 'oRegion' - AWS region, e.g. 'us-west-2'
--
-- * 'oOfferingType' - Offering type, e.g. 'NO_UPFRONT'
--
-- * 'oUsagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- * 'oFixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- * 'oDurationUnits' - Units for duration, e.g. 'MONTHS'
--
-- * 'oOfferingDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- * 'oDuration' - Lease duration, e.g. '12'
offering
    :: Offering
offering
  = Offering'{_oResourceSpecification = Nothing,
              _oCurrencyCode = Nothing, _oARN = Nothing,
              _oOfferingId = Nothing, _oRegion = Nothing,
              _oOfferingType = Nothing, _oUsagePrice = Nothing,
              _oFixedPrice = Nothing, _oDurationUnits = Nothing,
              _oOfferingDescription = Nothing,
              _oDuration = Nothing}

-- | Resource configuration details
oResourceSpecification :: Lens' Offering (Maybe ReservationResourceSpecification)
oResourceSpecification = lens _oResourceSpecification (\ s a -> s{_oResourceSpecification = a})

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
oCurrencyCode :: Lens' Offering (Maybe Text)
oCurrencyCode = lens _oCurrencyCode (\ s a -> s{_oCurrencyCode = a})

-- | Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
oARN :: Lens' Offering (Maybe Text)
oARN = lens _oARN (\ s a -> s{_oARN = a})

-- | Unique offering ID, e.g. '87654321'
oOfferingId :: Lens' Offering (Maybe Text)
oOfferingId = lens _oOfferingId (\ s a -> s{_oOfferingId = a})

-- | AWS region, e.g. 'us-west-2'
oRegion :: Lens' Offering (Maybe Text)
oRegion = lens _oRegion (\ s a -> s{_oRegion = a})

-- | Offering type, e.g. 'NO_UPFRONT'
oOfferingType :: Lens' Offering (Maybe OfferingType)
oOfferingType = lens _oOfferingType (\ s a -> s{_oOfferingType = a})

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
oUsagePrice :: Lens' Offering (Maybe Double)
oUsagePrice = lens _oUsagePrice (\ s a -> s{_oUsagePrice = a})

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
oFixedPrice :: Lens' Offering (Maybe Double)
oFixedPrice = lens _oFixedPrice (\ s a -> s{_oFixedPrice = a})

-- | Units for duration, e.g. 'MONTHS'
oDurationUnits :: Lens' Offering (Maybe OfferingDurationUnits)
oDurationUnits = lens _oDurationUnits (\ s a -> s{_oDurationUnits = a})

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
oOfferingDescription :: Lens' Offering (Maybe Text)
oOfferingDescription = lens _oOfferingDescription (\ s a -> s{_oOfferingDescription = a})

-- | Lease duration, e.g. '12'
oDuration :: Lens' Offering (Maybe Int)
oDuration = lens _oDuration (\ s a -> s{_oDuration = a})

instance FromJSON Offering where
        parseJSON
          = withObject "Offering"
              (\ x ->
                 Offering' <$>
                   (x .:? "resourceSpecification") <*>
                     (x .:? "currencyCode")
                     <*> (x .:? "arn")
                     <*> (x .:? "offeringId")
                     <*> (x .:? "region")
                     <*> (x .:? "offeringType")
                     <*> (x .:? "usagePrice")
                     <*> (x .:? "fixedPrice")
                     <*> (x .:? "durationUnits")
                     <*> (x .:? "offeringDescription")
                     <*> (x .:? "duration"))

instance Hashable Offering where

instance NFData Offering where

-- | Output settings. There can be multiple outputs within a group.
--
-- /See:/ 'output' smart constructor.
data Output = Output'{_oCaptionDescriptionNames ::
                      !(Maybe [Text]),
                      _oVideoDescriptionName :: !(Maybe Text),
                      _oOutputName :: !(Maybe Text),
                      _oAudioDescriptionNames :: !(Maybe [Text]),
                      _oOutputSettings :: !OutputSettings}
                deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Output' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oCaptionDescriptionNames' - The names of the CaptionDescriptions used as caption sources for this output.
--
-- * 'oVideoDescriptionName' - The name of the VideoDescription used as the source for this output.
--
-- * 'oOutputName' - The name used to identify an output.
--
-- * 'oAudioDescriptionNames' - The names of the AudioDescriptions used as audio sources for this output.
--
-- * 'oOutputSettings' - Output type-specific settings.
output
    :: OutputSettings -- ^ 'oOutputSettings'
    -> Output
output pOutputSettings_
  = Output'{_oCaptionDescriptionNames = Nothing,
            _oVideoDescriptionName = Nothing,
            _oOutputName = Nothing,
            _oAudioDescriptionNames = Nothing,
            _oOutputSettings = pOutputSettings_}

-- | The names of the CaptionDescriptions used as caption sources for this output.
oCaptionDescriptionNames :: Lens' Output [Text]
oCaptionDescriptionNames = lens _oCaptionDescriptionNames (\ s a -> s{_oCaptionDescriptionNames = a}) . _Default . _Coerce

-- | The name of the VideoDescription used as the source for this output.
oVideoDescriptionName :: Lens' Output (Maybe Text)
oVideoDescriptionName = lens _oVideoDescriptionName (\ s a -> s{_oVideoDescriptionName = a})

-- | The name used to identify an output.
oOutputName :: Lens' Output (Maybe Text)
oOutputName = lens _oOutputName (\ s a -> s{_oOutputName = a})

-- | The names of the AudioDescriptions used as audio sources for this output.
oAudioDescriptionNames :: Lens' Output [Text]
oAudioDescriptionNames = lens _oAudioDescriptionNames (\ s a -> s{_oAudioDescriptionNames = a}) . _Default . _Coerce

-- | Output type-specific settings.
oOutputSettings :: Lens' Output OutputSettings
oOutputSettings = lens _oOutputSettings (\ s a -> s{_oOutputSettings = a})

instance FromJSON Output where
        parseJSON
          = withObject "Output"
              (\ x ->
                 Output' <$>
                   (x .:? "captionDescriptionNames" .!= mempty) <*>
                     (x .:? "videoDescriptionName")
                     <*> (x .:? "outputName")
                     <*> (x .:? "audioDescriptionNames" .!= mempty)
                     <*> (x .: "outputSettings"))

instance Hashable Output where

instance NFData Output where

instance ToJSON Output where
        toJSON Output'{..}
          = object
              (catMaybes
                 [("captionDescriptionNames" .=) <$>
                    _oCaptionDescriptionNames,
                  ("videoDescriptionName" .=) <$>
                    _oVideoDescriptionName,
                  ("outputName" .=) <$> _oOutputName,
                  ("audioDescriptionNames" .=) <$>
                    _oAudioDescriptionNames,
                  Just ("outputSettings" .= _oOutputSettings)])

-- | Placeholder documentation for OutputDestination
--
-- /See:/ 'outputDestination' smart constructor.
data OutputDestination = OutputDestination'{_odSettings
                                            ::
                                            !(Maybe
                                                [OutputDestinationSettings]),
                                            _odMediaPackageSettings ::
                                            !(Maybe
                                                [MediaPackageOutputDestinationSettings]),
                                            _odId :: !(Maybe Text),
                                            _odMultiplexSettings ::
                                            !(Maybe
                                                MultiplexProgramChannelDestinationSettings)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odSettings' - Destination settings for a standard output; one destination for each redundant encoder.
--
-- * 'odMediaPackageSettings' - Destination settings for a MediaPackage output; one destination for both encoders.
--
-- * 'odId' - User-specified id. This is used in an output group or an output.
--
-- * 'odMultiplexSettings' - Destination settings for a Multiplex output; one destination for both encoders.
outputDestination
    :: OutputDestination
outputDestination
  = OutputDestination'{_odSettings = Nothing,
                       _odMediaPackageSettings = Nothing, _odId = Nothing,
                       _odMultiplexSettings = Nothing}

-- | Destination settings for a standard output; one destination for each redundant encoder.
odSettings :: Lens' OutputDestination [OutputDestinationSettings]
odSettings = lens _odSettings (\ s a -> s{_odSettings = a}) . _Default . _Coerce

-- | Destination settings for a MediaPackage output; one destination for both encoders.
odMediaPackageSettings :: Lens' OutputDestination [MediaPackageOutputDestinationSettings]
odMediaPackageSettings = lens _odMediaPackageSettings (\ s a -> s{_odMediaPackageSettings = a}) . _Default . _Coerce

-- | User-specified id. This is used in an output group or an output.
odId :: Lens' OutputDestination (Maybe Text)
odId = lens _odId (\ s a -> s{_odId = a})

-- | Destination settings for a Multiplex output; one destination for both encoders.
odMultiplexSettings :: Lens' OutputDestination (Maybe MultiplexProgramChannelDestinationSettings)
odMultiplexSettings = lens _odMultiplexSettings (\ s a -> s{_odMultiplexSettings = a})

instance FromJSON OutputDestination where
        parseJSON
          = withObject "OutputDestination"
              (\ x ->
                 OutputDestination' <$>
                   (x .:? "settings" .!= mempty) <*>
                     (x .:? "mediaPackageSettings" .!= mempty)
                     <*> (x .:? "id")
                     <*> (x .:? "multiplexSettings"))

instance Hashable OutputDestination where

instance NFData OutputDestination where

instance ToJSON OutputDestination where
        toJSON OutputDestination'{..}
          = object
              (catMaybes
                 [("settings" .=) <$> _odSettings,
                  ("mediaPackageSettings" .=) <$>
                    _odMediaPackageSettings,
                  ("id" .=) <$> _odId,
                  ("multiplexSettings" .=) <$> _odMultiplexSettings])

-- | Placeholder documentation for OutputDestinationSettings
--
-- /See:/ 'outputDestinationSettings' smart constructor.
data OutputDestinationSettings = OutputDestinationSettings'{_odsURL
                                                            :: !(Maybe Text),
                                                            _odsUsername ::
                                                            !(Maybe Text),
                                                            _odsPasswordParam ::
                                                            !(Maybe Text),
                                                            _odsStreamName ::
                                                            !(Maybe Text)}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'OutputDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odsURL' - A URL specifying a destination
--
-- * 'odsUsername' - username for destination
--
-- * 'odsPasswordParam' - key used to extract the password from EC2 Parameter store
--
-- * 'odsStreamName' - Stream name for RTMP destinations (URLs of type rtmp://)
outputDestinationSettings
    :: OutputDestinationSettings
outputDestinationSettings
  = OutputDestinationSettings'{_odsURL = Nothing,
                               _odsUsername = Nothing,
                               _odsPasswordParam = Nothing,
                               _odsStreamName = Nothing}

-- | A URL specifying a destination
odsURL :: Lens' OutputDestinationSettings (Maybe Text)
odsURL = lens _odsURL (\ s a -> s{_odsURL = a})

-- | username for destination
odsUsername :: Lens' OutputDestinationSettings (Maybe Text)
odsUsername = lens _odsUsername (\ s a -> s{_odsUsername = a})

-- | key used to extract the password from EC2 Parameter store
odsPasswordParam :: Lens' OutputDestinationSettings (Maybe Text)
odsPasswordParam = lens _odsPasswordParam (\ s a -> s{_odsPasswordParam = a})

-- | Stream name for RTMP destinations (URLs of type rtmp://)
odsStreamName :: Lens' OutputDestinationSettings (Maybe Text)
odsStreamName = lens _odsStreamName (\ s a -> s{_odsStreamName = a})

instance FromJSON OutputDestinationSettings where
        parseJSON
          = withObject "OutputDestinationSettings"
              (\ x ->
                 OutputDestinationSettings' <$>
                   (x .:? "url") <*> (x .:? "username") <*>
                     (x .:? "passwordParam")
                     <*> (x .:? "streamName"))

instance Hashable OutputDestinationSettings where

instance NFData OutputDestinationSettings where

instance ToJSON OutputDestinationSettings where
        toJSON OutputDestinationSettings'{..}
          = object
              (catMaybes
                 [("url" .=) <$> _odsURL,
                  ("username" .=) <$> _odsUsername,
                  ("passwordParam" .=) <$> _odsPasswordParam,
                  ("streamName" .=) <$> _odsStreamName])

-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
--
-- /See:/ 'outputGroup' smart constructor.
data OutputGroup = OutputGroup'{_ogName ::
                                !(Maybe Text),
                                _ogOutputs :: ![Output],
                                _ogOutputGroupSettings :: !OutputGroupSettings}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogName' - Custom output group name optionally defined by the user.  Only letters, numbers, and the underscore character allowed; only 32 characters allowed.
--
-- * 'ogOutputs' - Undocumented member.
--
-- * 'ogOutputGroupSettings' - Settings associated with the output group.
outputGroup
    :: OutputGroupSettings -- ^ 'ogOutputGroupSettings'
    -> OutputGroup
outputGroup pOutputGroupSettings_
  = OutputGroup'{_ogName = Nothing,
                 _ogOutputs = mempty,
                 _ogOutputGroupSettings = pOutputGroupSettings_}

-- | Custom output group name optionally defined by the user.  Only letters, numbers, and the underscore character allowed; only 32 characters allowed.
ogName :: Lens' OutputGroup (Maybe Text)
ogName = lens _ogName (\ s a -> s{_ogName = a})

-- | Undocumented member.
ogOutputs :: Lens' OutputGroup [Output]
ogOutputs = lens _ogOutputs (\ s a -> s{_ogOutputs = a}) . _Coerce

-- | Settings associated with the output group.
ogOutputGroupSettings :: Lens' OutputGroup OutputGroupSettings
ogOutputGroupSettings = lens _ogOutputGroupSettings (\ s a -> s{_ogOutputGroupSettings = a})

instance FromJSON OutputGroup where
        parseJSON
          = withObject "OutputGroup"
              (\ x ->
                 OutputGroup' <$>
                   (x .:? "name") <*> (x .:? "outputs" .!= mempty) <*>
                     (x .: "outputGroupSettings"))

instance Hashable OutputGroup where

instance NFData OutputGroup where

instance ToJSON OutputGroup where
        toJSON OutputGroup'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _ogName,
                  Just ("outputs" .= _ogOutputs),
                  Just
                    ("outputGroupSettings" .= _ogOutputGroupSettings)])

-- | Output Group Settings
--
-- /See:/ 'outputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'{_ogsMediaPackageGroupSettings
                                                ::
                                                !(Maybe
                                                    MediaPackageGroupSettings),
                                                _ogsMsSmoothGroupSettings ::
                                                !(Maybe MsSmoothGroupSettings),
                                                _ogsRtmpGroupSettings ::
                                                !(Maybe RtmpGroupSettings),
                                                _ogsMultiplexGroupSettings ::
                                                !(Maybe MultiplexGroupSettings),
                                                _ogsHlsGroupSettings ::
                                                !(Maybe HlsGroupSettings),
                                                _ogsArchiveGroupSettings ::
                                                !(Maybe ArchiveGroupSettings),
                                                _ogsUdpGroupSettings ::
                                                !(Maybe UdpGroupSettings),
                                                _ogsFrameCaptureGroupSettings ::
                                                !(Maybe
                                                    FrameCaptureGroupSettings)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogsMediaPackageGroupSettings' - Undocumented member.
--
-- * 'ogsMsSmoothGroupSettings' - Undocumented member.
--
-- * 'ogsRtmpGroupSettings' - Undocumented member.
--
-- * 'ogsMultiplexGroupSettings' - Undocumented member.
--
-- * 'ogsHlsGroupSettings' - Undocumented member.
--
-- * 'ogsArchiveGroupSettings' - Undocumented member.
--
-- * 'ogsUdpGroupSettings' - Undocumented member.
--
-- * 'ogsFrameCaptureGroupSettings' - Undocumented member.
outputGroupSettings
    :: OutputGroupSettings
outputGroupSettings
  = OutputGroupSettings'{_ogsMediaPackageGroupSettings
                           = Nothing,
                         _ogsMsSmoothGroupSettings = Nothing,
                         _ogsRtmpGroupSettings = Nothing,
                         _ogsMultiplexGroupSettings = Nothing,
                         _ogsHlsGroupSettings = Nothing,
                         _ogsArchiveGroupSettings = Nothing,
                         _ogsUdpGroupSettings = Nothing,
                         _ogsFrameCaptureGroupSettings = Nothing}

-- | Undocumented member.
ogsMediaPackageGroupSettings :: Lens' OutputGroupSettings (Maybe MediaPackageGroupSettings)
ogsMediaPackageGroupSettings = lens _ogsMediaPackageGroupSettings (\ s a -> s{_ogsMediaPackageGroupSettings = a})

-- | Undocumented member.
ogsMsSmoothGroupSettings :: Lens' OutputGroupSettings (Maybe MsSmoothGroupSettings)
ogsMsSmoothGroupSettings = lens _ogsMsSmoothGroupSettings (\ s a -> s{_ogsMsSmoothGroupSettings = a})

-- | Undocumented member.
ogsRtmpGroupSettings :: Lens' OutputGroupSettings (Maybe RtmpGroupSettings)
ogsRtmpGroupSettings = lens _ogsRtmpGroupSettings (\ s a -> s{_ogsRtmpGroupSettings = a})

-- | Undocumented member.
ogsMultiplexGroupSettings :: Lens' OutputGroupSettings (Maybe MultiplexGroupSettings)
ogsMultiplexGroupSettings = lens _ogsMultiplexGroupSettings (\ s a -> s{_ogsMultiplexGroupSettings = a})

-- | Undocumented member.
ogsHlsGroupSettings :: Lens' OutputGroupSettings (Maybe HlsGroupSettings)
ogsHlsGroupSettings = lens _ogsHlsGroupSettings (\ s a -> s{_ogsHlsGroupSettings = a})

-- | Undocumented member.
ogsArchiveGroupSettings :: Lens' OutputGroupSettings (Maybe ArchiveGroupSettings)
ogsArchiveGroupSettings = lens _ogsArchiveGroupSettings (\ s a -> s{_ogsArchiveGroupSettings = a})

-- | Undocumented member.
ogsUdpGroupSettings :: Lens' OutputGroupSettings (Maybe UdpGroupSettings)
ogsUdpGroupSettings = lens _ogsUdpGroupSettings (\ s a -> s{_ogsUdpGroupSettings = a})

-- | Undocumented member.
ogsFrameCaptureGroupSettings :: Lens' OutputGroupSettings (Maybe FrameCaptureGroupSettings)
ogsFrameCaptureGroupSettings = lens _ogsFrameCaptureGroupSettings (\ s a -> s{_ogsFrameCaptureGroupSettings = a})

instance FromJSON OutputGroupSettings where
        parseJSON
          = withObject "OutputGroupSettings"
              (\ x ->
                 OutputGroupSettings' <$>
                   (x .:? "mediaPackageGroupSettings") <*>
                     (x .:? "msSmoothGroupSettings")
                     <*> (x .:? "rtmpGroupSettings")
                     <*> (x .:? "multiplexGroupSettings")
                     <*> (x .:? "hlsGroupSettings")
                     <*> (x .:? "archiveGroupSettings")
                     <*> (x .:? "udpGroupSettings")
                     <*> (x .:? "frameCaptureGroupSettings"))

instance Hashable OutputGroupSettings where

instance NFData OutputGroupSettings where

instance ToJSON OutputGroupSettings where
        toJSON OutputGroupSettings'{..}
          = object
              (catMaybes
                 [("mediaPackageGroupSettings" .=) <$>
                    _ogsMediaPackageGroupSettings,
                  ("msSmoothGroupSettings" .=) <$>
                    _ogsMsSmoothGroupSettings,
                  ("rtmpGroupSettings" .=) <$> _ogsRtmpGroupSettings,
                  ("multiplexGroupSettings" .=) <$>
                    _ogsMultiplexGroupSettings,
                  ("hlsGroupSettings" .=) <$> _ogsHlsGroupSettings,
                  ("archiveGroupSettings" .=) <$>
                    _ogsArchiveGroupSettings,
                  ("udpGroupSettings" .=) <$> _ogsUdpGroupSettings,
                  ("frameCaptureGroupSettings" .=) <$>
                    _ogsFrameCaptureGroupSettings])

-- | Reference to an OutputDestination ID defined in the channel
--
-- /See:/ 'outputLocationRef' smart constructor.
newtype OutputLocationRef = OutputLocationRef'{_olrDestinationRefId
                                               :: Maybe Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputLocationRef' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olrDestinationRefId' - Undocumented member.
outputLocationRef
    :: OutputLocationRef
outputLocationRef
  = OutputLocationRef'{_olrDestinationRefId = Nothing}

-- | Undocumented member.
olrDestinationRefId :: Lens' OutputLocationRef (Maybe Text)
olrDestinationRefId = lens _olrDestinationRefId (\ s a -> s{_olrDestinationRefId = a})

instance FromJSON OutputLocationRef where
        parseJSON
          = withObject "OutputLocationRef"
              (\ x ->
                 OutputLocationRef' <$> (x .:? "destinationRefId"))

instance Hashable OutputLocationRef where

instance NFData OutputLocationRef where

instance ToJSON OutputLocationRef where
        toJSON OutputLocationRef'{..}
          = object
              (catMaybes
                 [("destinationRefId" .=) <$> _olrDestinationRefId])

-- | Output Settings
--
-- /See:/ 'outputSettings' smart constructor.
data OutputSettings = OutputSettings'{_osMultiplexOutputSettings
                                      :: !(Maybe MultiplexOutputSettings),
                                      _osArchiveOutputSettings ::
                                      !(Maybe ArchiveOutputSettings),
                                      _osRtmpOutputSettings ::
                                      !(Maybe RtmpOutputSettings),
                                      _osMediaPackageOutputSettings ::
                                      !(Maybe MediaPackageOutputSettings),
                                      _osHlsOutputSettings ::
                                      !(Maybe HlsOutputSettings),
                                      _osFrameCaptureOutputSettings ::
                                      !(Maybe FrameCaptureOutputSettings),
                                      _osUdpOutputSettings ::
                                      !(Maybe UdpOutputSettings),
                                      _osMsSmoothOutputSettings ::
                                      !(Maybe MsSmoothOutputSettings)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osMultiplexOutputSettings' - Undocumented member.
--
-- * 'osArchiveOutputSettings' - Undocumented member.
--
-- * 'osRtmpOutputSettings' - Undocumented member.
--
-- * 'osMediaPackageOutputSettings' - Undocumented member.
--
-- * 'osHlsOutputSettings' - Undocumented member.
--
-- * 'osFrameCaptureOutputSettings' - Undocumented member.
--
-- * 'osUdpOutputSettings' - Undocumented member.
--
-- * 'osMsSmoothOutputSettings' - Undocumented member.
outputSettings
    :: OutputSettings
outputSettings
  = OutputSettings'{_osMultiplexOutputSettings =
                      Nothing,
                    _osArchiveOutputSettings = Nothing,
                    _osRtmpOutputSettings = Nothing,
                    _osMediaPackageOutputSettings = Nothing,
                    _osHlsOutputSettings = Nothing,
                    _osFrameCaptureOutputSettings = Nothing,
                    _osUdpOutputSettings = Nothing,
                    _osMsSmoothOutputSettings = Nothing}

-- | Undocumented member.
osMultiplexOutputSettings :: Lens' OutputSettings (Maybe MultiplexOutputSettings)
osMultiplexOutputSettings = lens _osMultiplexOutputSettings (\ s a -> s{_osMultiplexOutputSettings = a})

-- | Undocumented member.
osArchiveOutputSettings :: Lens' OutputSettings (Maybe ArchiveOutputSettings)
osArchiveOutputSettings = lens _osArchiveOutputSettings (\ s a -> s{_osArchiveOutputSettings = a})

-- | Undocumented member.
osRtmpOutputSettings :: Lens' OutputSettings (Maybe RtmpOutputSettings)
osRtmpOutputSettings = lens _osRtmpOutputSettings (\ s a -> s{_osRtmpOutputSettings = a})

-- | Undocumented member.
osMediaPackageOutputSettings :: Lens' OutputSettings (Maybe MediaPackageOutputSettings)
osMediaPackageOutputSettings = lens _osMediaPackageOutputSettings (\ s a -> s{_osMediaPackageOutputSettings = a})

-- | Undocumented member.
osHlsOutputSettings :: Lens' OutputSettings (Maybe HlsOutputSettings)
osHlsOutputSettings = lens _osHlsOutputSettings (\ s a -> s{_osHlsOutputSettings = a})

-- | Undocumented member.
osFrameCaptureOutputSettings :: Lens' OutputSettings (Maybe FrameCaptureOutputSettings)
osFrameCaptureOutputSettings = lens _osFrameCaptureOutputSettings (\ s a -> s{_osFrameCaptureOutputSettings = a})

-- | Undocumented member.
osUdpOutputSettings :: Lens' OutputSettings (Maybe UdpOutputSettings)
osUdpOutputSettings = lens _osUdpOutputSettings (\ s a -> s{_osUdpOutputSettings = a})

-- | Undocumented member.
osMsSmoothOutputSettings :: Lens' OutputSettings (Maybe MsSmoothOutputSettings)
osMsSmoothOutputSettings = lens _osMsSmoothOutputSettings (\ s a -> s{_osMsSmoothOutputSettings = a})

instance FromJSON OutputSettings where
        parseJSON
          = withObject "OutputSettings"
              (\ x ->
                 OutputSettings' <$>
                   (x .:? "multiplexOutputSettings") <*>
                     (x .:? "archiveOutputSettings")
                     <*> (x .:? "rtmpOutputSettings")
                     <*> (x .:? "mediaPackageOutputSettings")
                     <*> (x .:? "hlsOutputSettings")
                     <*> (x .:? "frameCaptureOutputSettings")
                     <*> (x .:? "udpOutputSettings")
                     <*> (x .:? "msSmoothOutputSettings"))

instance Hashable OutputSettings where

instance NFData OutputSettings where

instance ToJSON OutputSettings where
        toJSON OutputSettings'{..}
          = object
              (catMaybes
                 [("multiplexOutputSettings" .=) <$>
                    _osMultiplexOutputSettings,
                  ("archiveOutputSettings" .=) <$>
                    _osArchiveOutputSettings,
                  ("rtmpOutputSettings" .=) <$> _osRtmpOutputSettings,
                  ("mediaPackageOutputSettings" .=) <$>
                    _osMediaPackageOutputSettings,
                  ("hlsOutputSettings" .=) <$> _osHlsOutputSettings,
                  ("frameCaptureOutputSettings" .=) <$>
                    _osFrameCaptureOutputSettings,
                  ("udpOutputSettings" .=) <$> _osUdpOutputSettings,
                  ("msSmoothOutputSettings" .=) <$>
                    _osMsSmoothOutputSettings])

-- | Pass Through Settings
--
-- /See:/ 'passThroughSettings' smart constructor.
data PassThroughSettings = PassThroughSettings'
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PassThroughSettings' with the minimum fields required to make a request.
--
passThroughSettings
    :: PassThroughSettings
passThroughSettings = PassThroughSettings'

instance FromJSON PassThroughSettings where
        parseJSON
          = withObject "PassThroughSettings"
              (\ x -> pure PassThroughSettings')

instance Hashable PassThroughSettings where

instance NFData PassThroughSettings where

instance ToJSON PassThroughSettings where
        toJSON = const (Object mempty)

-- | Settings for the action to set pause state of a channel.
--
-- /See:/ 'pauseStateScheduleActionSettings' smart constructor.
newtype PauseStateScheduleActionSettings = PauseStateScheduleActionSettings'{_pssasPipelines
                                                                             ::
                                                                             Maybe
                                                                               [PipelinePauseStateSettings]}
                                             deriving (Eq, Read, Show, Data,
                                                       Typeable, Generic)

-- | Creates a value of 'PauseStateScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pssasPipelines' - Undocumented member.
pauseStateScheduleActionSettings
    :: PauseStateScheduleActionSettings
pauseStateScheduleActionSettings
  = PauseStateScheduleActionSettings'{_pssasPipelines =
                                        Nothing}

-- | Undocumented member.
pssasPipelines :: Lens' PauseStateScheduleActionSettings [PipelinePauseStateSettings]
pssasPipelines = lens _pssasPipelines (\ s a -> s{_pssasPipelines = a}) . _Default . _Coerce

instance FromJSON PauseStateScheduleActionSettings
         where
        parseJSON
          = withObject "PauseStateScheduleActionSettings"
              (\ x ->
                 PauseStateScheduleActionSettings' <$>
                   (x .:? "pipelines" .!= mempty))

instance Hashable PauseStateScheduleActionSettings
         where

instance NFData PauseStateScheduleActionSettings
         where

instance ToJSON PauseStateScheduleActionSettings
         where
        toJSON PauseStateScheduleActionSettings'{..}
          = object
              (catMaybes [("pipelines" .=) <$> _pssasPipelines])

-- | Runtime details of a pipeline when a channel is running.
--
-- /See:/ 'pipelineDetail' smart constructor.
data PipelineDetail = PipelineDetail'{_pdPipelineId
                                      :: !(Maybe Text),
                                      _pdActiveInputSwitchActionName ::
                                      !(Maybe Text),
                                      _pdActiveInputAttachmentName ::
                                      !(Maybe Text)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdPipelineId' - Pipeline ID
--
-- * 'pdActiveInputSwitchActionName' - The name of the input switch schedule action that occurred most recently and that resulted in the switch to the current input attachment for this pipeline.
--
-- * 'pdActiveInputAttachmentName' - The name of the active input attachment currently being ingested by this pipeline.
pipelineDetail
    :: PipelineDetail
pipelineDetail
  = PipelineDetail'{_pdPipelineId = Nothing,
                    _pdActiveInputSwitchActionName = Nothing,
                    _pdActiveInputAttachmentName = Nothing}

-- | Pipeline ID
pdPipelineId :: Lens' PipelineDetail (Maybe Text)
pdPipelineId = lens _pdPipelineId (\ s a -> s{_pdPipelineId = a})

-- | The name of the input switch schedule action that occurred most recently and that resulted in the switch to the current input attachment for this pipeline.
pdActiveInputSwitchActionName :: Lens' PipelineDetail (Maybe Text)
pdActiveInputSwitchActionName = lens _pdActiveInputSwitchActionName (\ s a -> s{_pdActiveInputSwitchActionName = a})

-- | The name of the active input attachment currently being ingested by this pipeline.
pdActiveInputAttachmentName :: Lens' PipelineDetail (Maybe Text)
pdActiveInputAttachmentName = lens _pdActiveInputAttachmentName (\ s a -> s{_pdActiveInputAttachmentName = a})

instance FromJSON PipelineDetail where
        parseJSON
          = withObject "PipelineDetail"
              (\ x ->
                 PipelineDetail' <$>
                   (x .:? "pipelineId") <*>
                     (x .:? "activeInputSwitchActionName")
                     <*> (x .:? "activeInputAttachmentName"))

instance Hashable PipelineDetail where

instance NFData PipelineDetail where

-- | Settings for pausing a pipeline.
--
-- /See:/ 'pipelinePauseStateSettings' smart constructor.
newtype PipelinePauseStateSettings = PipelinePauseStateSettings'{_ppssPipelineId
                                                                 :: PipelineId}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'PipelinePauseStateSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppssPipelineId' - Pipeline ID to pause ("PIPELINE_0" or "PIPELINE_1").
pipelinePauseStateSettings
    :: PipelineId -- ^ 'ppssPipelineId'
    -> PipelinePauseStateSettings
pipelinePauseStateSettings pPipelineId_
  = PipelinePauseStateSettings'{_ppssPipelineId =
                                  pPipelineId_}

-- | Pipeline ID to pause ("PIPELINE_0" or "PIPELINE_1").
ppssPipelineId :: Lens' PipelinePauseStateSettings PipelineId
ppssPipelineId = lens _ppssPipelineId (\ s a -> s{_ppssPipelineId = a})

instance FromJSON PipelinePauseStateSettings where
        parseJSON
          = withObject "PipelinePauseStateSettings"
              (\ x ->
                 PipelinePauseStateSettings' <$> (x .: "pipelineId"))

instance Hashable PipelinePauseStateSettings where

instance NFData PipelinePauseStateSettings where

instance ToJSON PipelinePauseStateSettings where
        toJSON PipelinePauseStateSettings'{..}
          = object
              (catMaybes [Just ("pipelineId" .= _ppssPipelineId)])

-- | Rec601 Settings
--
-- /See:/ 'rec601Settings' smart constructor.
data Rec601Settings = Rec601Settings'
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Rec601Settings' with the minimum fields required to make a request.
--
rec601Settings
    :: Rec601Settings
rec601Settings = Rec601Settings'

instance FromJSON Rec601Settings where
        parseJSON
          = withObject "Rec601Settings"
              (\ x -> pure Rec601Settings')

instance Hashable Rec601Settings where

instance NFData Rec601Settings where

instance ToJSON Rec601Settings where
        toJSON = const (Object mempty)

-- | Rec709 Settings
--
-- /See:/ 'rec709Settings' smart constructor.
data Rec709Settings = Rec709Settings'
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Rec709Settings' with the minimum fields required to make a request.
--
rec709Settings
    :: Rec709Settings
rec709Settings = Rec709Settings'

instance FromJSON Rec709Settings where
        parseJSON
          = withObject "Rec709Settings"
              (\ x -> pure Rec709Settings')

instance Hashable Rec709Settings where

instance NFData Rec709Settings where

instance ToJSON Rec709Settings where
        toJSON = const (Object mempty)

-- | Remix Settings
--
-- /See:/ 'remixSettings' smart constructor.
data RemixSettings = RemixSettings'{_rsChannelsIn ::
                                    !(Maybe Nat),
                                    _rsChannelsOut :: !(Maybe Nat),
                                    _rsChannelMappings ::
                                    ![AudioChannelMapping]}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemixSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsChannelsIn' - Number of input channels to be used.
--
-- * 'rsChannelsOut' - Number of output channels to be produced. Valid values: 1, 2, 4, 6, 8
--
-- * 'rsChannelMappings' - Mapping of input channels to output channels, with appropriate gain adjustments.
remixSettings
    :: RemixSettings
remixSettings
  = RemixSettings'{_rsChannelsIn = Nothing,
                   _rsChannelsOut = Nothing,
                   _rsChannelMappings = mempty}

-- | Number of input channels to be used.
rsChannelsIn :: Lens' RemixSettings (Maybe Natural)
rsChannelsIn = lens _rsChannelsIn (\ s a -> s{_rsChannelsIn = a}) . mapping _Nat

-- | Number of output channels to be produced. Valid values: 1, 2, 4, 6, 8
rsChannelsOut :: Lens' RemixSettings (Maybe Natural)
rsChannelsOut = lens _rsChannelsOut (\ s a -> s{_rsChannelsOut = a}) . mapping _Nat

-- | Mapping of input channels to output channels, with appropriate gain adjustments.
rsChannelMappings :: Lens' RemixSettings [AudioChannelMapping]
rsChannelMappings = lens _rsChannelMappings (\ s a -> s{_rsChannelMappings = a}) . _Coerce

instance FromJSON RemixSettings where
        parseJSON
          = withObject "RemixSettings"
              (\ x ->
                 RemixSettings' <$>
                   (x .:? "channelsIn") <*> (x .:? "channelsOut") <*>
                     (x .:? "channelMappings" .!= mempty))

instance Hashable RemixSettings where

instance NFData RemixSettings where

instance ToJSON RemixSettings where
        toJSON RemixSettings'{..}
          = object
              (catMaybes
                 [("channelsIn" .=) <$> _rsChannelsIn,
                  ("channelsOut" .=) <$> _rsChannelsOut,
                  Just ("channelMappings" .= _rsChannelMappings)])

-- | Reserved resources available to use
--
-- /See:/ 'reservation' smart constructor.
data Reservation = Reservation'{_rState ::
                                !(Maybe ReservationState),
                                _rResourceSpecification ::
                                !(Maybe ReservationResourceSpecification),
                                _rCurrencyCode :: !(Maybe Text),
                                _rARN :: !(Maybe Text),
                                _rStart :: !(Maybe Text),
                                _rCount :: !(Maybe Int), _rEnd :: !(Maybe Text),
                                _rName :: !(Maybe Text),
                                _rReservationId :: !(Maybe Text),
                                _rOfferingId :: !(Maybe Text),
                                _rRegion :: !(Maybe Text),
                                _rOfferingType :: !(Maybe OfferingType),
                                _rUsagePrice :: !(Maybe Double),
                                _rFixedPrice :: !(Maybe Double),
                                _rDurationUnits ::
                                !(Maybe OfferingDurationUnits),
                                _rOfferingDescription :: !(Maybe Text),
                                _rDuration :: !(Maybe Int),
                                _rTags :: !(Maybe (Map Text Text))}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Reservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rState' - Current state of reservation, e.g. 'ACTIVE'
--
-- * 'rResourceSpecification' - Resource configuration details
--
-- * 'rCurrencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- * 'rARN' - Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- * 'rStart' - Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- * 'rCount' - Number of reserved resources
--
-- * 'rEnd' - Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- * 'rName' - User specified reservation name
--
-- * 'rReservationId' - Unique reservation ID, e.g. '1234567'
--
-- * 'rOfferingId' - Unique offering ID, e.g. '87654321'
--
-- * 'rRegion' - AWS region, e.g. 'us-west-2'
--
-- * 'rOfferingType' - Offering type, e.g. 'NO_UPFRONT'
--
-- * 'rUsagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- * 'rFixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- * 'rDurationUnits' - Units for duration, e.g. 'MONTHS'
--
-- * 'rOfferingDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- * 'rDuration' - Lease duration, e.g. '12'
--
-- * 'rTags' - A collection of key-value pairs
reservation
    :: Reservation
reservation
  = Reservation'{_rState = Nothing,
                 _rResourceSpecification = Nothing,
                 _rCurrencyCode = Nothing, _rARN = Nothing,
                 _rStart = Nothing, _rCount = Nothing,
                 _rEnd = Nothing, _rName = Nothing,
                 _rReservationId = Nothing, _rOfferingId = Nothing,
                 _rRegion = Nothing, _rOfferingType = Nothing,
                 _rUsagePrice = Nothing, _rFixedPrice = Nothing,
                 _rDurationUnits = Nothing,
                 _rOfferingDescription = Nothing,
                 _rDuration = Nothing, _rTags = Nothing}

-- | Current state of reservation, e.g. 'ACTIVE'
rState :: Lens' Reservation (Maybe ReservationState)
rState = lens _rState (\ s a -> s{_rState = a})

-- | Resource configuration details
rResourceSpecification :: Lens' Reservation (Maybe ReservationResourceSpecification)
rResourceSpecification = lens _rResourceSpecification (\ s a -> s{_rResourceSpecification = a})

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
rCurrencyCode :: Lens' Reservation (Maybe Text)
rCurrencyCode = lens _rCurrencyCode (\ s a -> s{_rCurrencyCode = a})

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
rARN :: Lens' Reservation (Maybe Text)
rARN = lens _rARN (\ s a -> s{_rARN = a})

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
rStart :: Lens' Reservation (Maybe Text)
rStart = lens _rStart (\ s a -> s{_rStart = a})

-- | Number of reserved resources
rCount :: Lens' Reservation (Maybe Int)
rCount = lens _rCount (\ s a -> s{_rCount = a})

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
rEnd :: Lens' Reservation (Maybe Text)
rEnd = lens _rEnd (\ s a -> s{_rEnd = a})

-- | User specified reservation name
rName :: Lens' Reservation (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a})

-- | Unique reservation ID, e.g. '1234567'
rReservationId :: Lens' Reservation (Maybe Text)
rReservationId = lens _rReservationId (\ s a -> s{_rReservationId = a})

-- | Unique offering ID, e.g. '87654321'
rOfferingId :: Lens' Reservation (Maybe Text)
rOfferingId = lens _rOfferingId (\ s a -> s{_rOfferingId = a})

-- | AWS region, e.g. 'us-west-2'
rRegion :: Lens' Reservation (Maybe Text)
rRegion = lens _rRegion (\ s a -> s{_rRegion = a})

-- | Offering type, e.g. 'NO_UPFRONT'
rOfferingType :: Lens' Reservation (Maybe OfferingType)
rOfferingType = lens _rOfferingType (\ s a -> s{_rOfferingType = a})

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
rUsagePrice :: Lens' Reservation (Maybe Double)
rUsagePrice = lens _rUsagePrice (\ s a -> s{_rUsagePrice = a})

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
rFixedPrice :: Lens' Reservation (Maybe Double)
rFixedPrice = lens _rFixedPrice (\ s a -> s{_rFixedPrice = a})

-- | Units for duration, e.g. 'MONTHS'
rDurationUnits :: Lens' Reservation (Maybe OfferingDurationUnits)
rDurationUnits = lens _rDurationUnits (\ s a -> s{_rDurationUnits = a})

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
rOfferingDescription :: Lens' Reservation (Maybe Text)
rOfferingDescription = lens _rOfferingDescription (\ s a -> s{_rOfferingDescription = a})

-- | Lease duration, e.g. '12'
rDuration :: Lens' Reservation (Maybe Int)
rDuration = lens _rDuration (\ s a -> s{_rDuration = a})

-- | A collection of key-value pairs
rTags :: Lens' Reservation (HashMap Text Text)
rTags = lens _rTags (\ s a -> s{_rTags = a}) . _Default . _Map

instance FromJSON Reservation where
        parseJSON
          = withObject "Reservation"
              (\ x ->
                 Reservation' <$>
                   (x .:? "state") <*> (x .:? "resourceSpecification")
                     <*> (x .:? "currencyCode")
                     <*> (x .:? "arn")
                     <*> (x .:? "start")
                     <*> (x .:? "count")
                     <*> (x .:? "end")
                     <*> (x .:? "name")
                     <*> (x .:? "reservationId")
                     <*> (x .:? "offeringId")
                     <*> (x .:? "region")
                     <*> (x .:? "offeringType")
                     <*> (x .:? "usagePrice")
                     <*> (x .:? "fixedPrice")
                     <*> (x .:? "durationUnits")
                     <*> (x .:? "offeringDescription")
                     <*> (x .:? "duration")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable Reservation where

instance NFData Reservation where

-- | Resource configuration (codec, resolution, bitrate, ...)
--
-- /See:/ 'reservationResourceSpecification' smart constructor.
data ReservationResourceSpecification = ReservationResourceSpecification'{_rrsVideoQuality
                                                                          ::
                                                                          !(Maybe
                                                                              ReservationVideoQuality),
                                                                          _rrsMaximumFramerate
                                                                          ::
                                                                          !(Maybe
                                                                              ReservationMaximumFramerate),
                                                                          _rrsResourceType
                                                                          ::
                                                                          !(Maybe
                                                                              ReservationResourceType),
                                                                          _rrsResolution
                                                                          ::
                                                                          !(Maybe
                                                                              ReservationResolution),
                                                                          _rrsCodec
                                                                          ::
                                                                          !(Maybe
                                                                              ReservationCodec),
                                                                          _rrsSpecialFeature
                                                                          ::
                                                                          !(Maybe
                                                                              ReservationSpecialFeature),
                                                                          _rrsChannelClass
                                                                          ::
                                                                          !(Maybe
                                                                              ChannelClass),
                                                                          _rrsMaximumBitrate
                                                                          ::
                                                                          !(Maybe
                                                                              ReservationMaximumBitrate)}
                                          deriving (Eq, Read, Show, Data,
                                                    Typeable, Generic)

-- | Creates a value of 'ReservationResourceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsVideoQuality' - Video quality, e.g. 'STANDARD' (Outputs only)
--
-- * 'rrsMaximumFramerate' - Maximum framerate, e.g. 'MAX_30_FPS' (Outputs only)
--
-- * 'rrsResourceType' - Resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
--
-- * 'rrsResolution' - Resolution, e.g. 'HD'
--
-- * 'rrsCodec' - Codec, e.g. 'AVC'
--
-- * 'rrsSpecialFeature' - Special feature, e.g. 'AUDIO_NORMALIZATION' (Channels only)
--
-- * 'rrsChannelClass' - Channel class, e.g. 'STANDARD'
--
-- * 'rrsMaximumBitrate' - Maximum bitrate, e.g. 'MAX_20_MBPS'
reservationResourceSpecification
    :: ReservationResourceSpecification
reservationResourceSpecification
  = ReservationResourceSpecification'{_rrsVideoQuality
                                        = Nothing,
                                      _rrsMaximumFramerate = Nothing,
                                      _rrsResourceType = Nothing,
                                      _rrsResolution = Nothing,
                                      _rrsCodec = Nothing,
                                      _rrsSpecialFeature = Nothing,
                                      _rrsChannelClass = Nothing,
                                      _rrsMaximumBitrate = Nothing}

-- | Video quality, e.g. 'STANDARD' (Outputs only)
rrsVideoQuality :: Lens' ReservationResourceSpecification (Maybe ReservationVideoQuality)
rrsVideoQuality = lens _rrsVideoQuality (\ s a -> s{_rrsVideoQuality = a})

-- | Maximum framerate, e.g. 'MAX_30_FPS' (Outputs only)
rrsMaximumFramerate :: Lens' ReservationResourceSpecification (Maybe ReservationMaximumFramerate)
rrsMaximumFramerate = lens _rrsMaximumFramerate (\ s a -> s{_rrsMaximumFramerate = a})

-- | Resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
rrsResourceType :: Lens' ReservationResourceSpecification (Maybe ReservationResourceType)
rrsResourceType = lens _rrsResourceType (\ s a -> s{_rrsResourceType = a})

-- | Resolution, e.g. 'HD'
rrsResolution :: Lens' ReservationResourceSpecification (Maybe ReservationResolution)
rrsResolution = lens _rrsResolution (\ s a -> s{_rrsResolution = a})

-- | Codec, e.g. 'AVC'
rrsCodec :: Lens' ReservationResourceSpecification (Maybe ReservationCodec)
rrsCodec = lens _rrsCodec (\ s a -> s{_rrsCodec = a})

-- | Special feature, e.g. 'AUDIO_NORMALIZATION' (Channels only)
rrsSpecialFeature :: Lens' ReservationResourceSpecification (Maybe ReservationSpecialFeature)
rrsSpecialFeature = lens _rrsSpecialFeature (\ s a -> s{_rrsSpecialFeature = a})

-- | Channel class, e.g. 'STANDARD'
rrsChannelClass :: Lens' ReservationResourceSpecification (Maybe ChannelClass)
rrsChannelClass = lens _rrsChannelClass (\ s a -> s{_rrsChannelClass = a})

-- | Maximum bitrate, e.g. 'MAX_20_MBPS'
rrsMaximumBitrate :: Lens' ReservationResourceSpecification (Maybe ReservationMaximumBitrate)
rrsMaximumBitrate = lens _rrsMaximumBitrate (\ s a -> s{_rrsMaximumBitrate = a})

instance FromJSON ReservationResourceSpecification
         where
        parseJSON
          = withObject "ReservationResourceSpecification"
              (\ x ->
                 ReservationResourceSpecification' <$>
                   (x .:? "videoQuality") <*> (x .:? "maximumFramerate")
                     <*> (x .:? "resourceType")
                     <*> (x .:? "resolution")
                     <*> (x .:? "codec")
                     <*> (x .:? "specialFeature")
                     <*> (x .:? "channelClass")
                     <*> (x .:? "maximumBitrate"))

instance Hashable ReservationResourceSpecification
         where

instance NFData ReservationResourceSpecification
         where

-- | Rtmp Caption Info Destination Settings
--
-- /See:/ 'rtmpCaptionInfoDestinationSettings' smart constructor.
data RtmpCaptionInfoDestinationSettings = RtmpCaptionInfoDestinationSettings'
                                            deriving (Eq, Read, Show, Data,
                                                      Typeable, Generic)

-- | Creates a value of 'RtmpCaptionInfoDestinationSettings' with the minimum fields required to make a request.
--
rtmpCaptionInfoDestinationSettings
    :: RtmpCaptionInfoDestinationSettings
rtmpCaptionInfoDestinationSettings
  = RtmpCaptionInfoDestinationSettings'

instance FromJSON RtmpCaptionInfoDestinationSettings
         where
        parseJSON
          = withObject "RtmpCaptionInfoDestinationSettings"
              (\ x -> pure RtmpCaptionInfoDestinationSettings')

instance Hashable RtmpCaptionInfoDestinationSettings
         where

instance NFData RtmpCaptionInfoDestinationSettings
         where

instance ToJSON RtmpCaptionInfoDestinationSettings
         where
        toJSON = const (Object mempty)

-- | Rtmp Group Settings
--
-- /See:/ 'rtmpGroupSettings' smart constructor.
data RtmpGroupSettings = RtmpGroupSettings'{_rgsInputLossAction
                                            ::
                                            !(Maybe InputLossActionForRtmpOut),
                                            _rgsCaptionData ::
                                            !(Maybe RtmpCaptionData),
                                            _rgsRestartDelay :: !(Maybe Nat),
                                            _rgsAuthenticationScheme ::
                                            !(Maybe AuthenticationScheme),
                                            _rgsCacheLength :: !(Maybe Nat),
                                            _rgsCacheFullBehavior ::
                                            !(Maybe RtmpCacheFullBehavior)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RtmpGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsInputLossAction' - Controls the behavior of this RTMP group if input becomes unavailable. - emitOutput: Emit a slate until input returns. - pauseOutput: Stop transmitting data until input returns. This does not close the underlying RTMP connection.
--
-- * 'rgsCaptionData' - Controls the types of data that passes to onCaptionInfo outputs.  If set to 'all' then 608 and 708 carried DTVCC data will be passed.  If set to 'field1AndField2608' then DTVCC data will be stripped out, but 608 data from both fields will be passed. If set to 'field1608' then only the data carried in 608 from field 1 video will be passed.
--
-- * 'rgsRestartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- * 'rgsAuthenticationScheme' - Authentication scheme to use when connecting with CDN
--
-- * 'rgsCacheLength' - Cache length, in seconds, is used to calculate buffer size.
--
-- * 'rgsCacheFullBehavior' - Controls behavior when content cache fills up. If remote origin server stalls the RTMP connection and does not accept content fast enough the 'Media Cache' will fill up. When the cache reaches the duration specified by cacheLength the cache will stop accepting new content. If set to disconnectImmediately, the RTMP output will force a disconnect. Clear the media cache, and reconnect after restartDelay seconds. If set to waitForServer, the RTMP output will wait up to 5 minutes to allow the origin server to begin accepting data again.
rtmpGroupSettings
    :: RtmpGroupSettings
rtmpGroupSettings
  = RtmpGroupSettings'{_rgsInputLossAction = Nothing,
                       _rgsCaptionData = Nothing,
                       _rgsRestartDelay = Nothing,
                       _rgsAuthenticationScheme = Nothing,
                       _rgsCacheLength = Nothing,
                       _rgsCacheFullBehavior = Nothing}

-- | Controls the behavior of this RTMP group if input becomes unavailable. - emitOutput: Emit a slate until input returns. - pauseOutput: Stop transmitting data until input returns. This does not close the underlying RTMP connection.
rgsInputLossAction :: Lens' RtmpGroupSettings (Maybe InputLossActionForRtmpOut)
rgsInputLossAction = lens _rgsInputLossAction (\ s a -> s{_rgsInputLossAction = a})

-- | Controls the types of data that passes to onCaptionInfo outputs.  If set to 'all' then 608 and 708 carried DTVCC data will be passed.  If set to 'field1AndField2608' then DTVCC data will be stripped out, but 608 data from both fields will be passed. If set to 'field1608' then only the data carried in 608 from field 1 video will be passed.
rgsCaptionData :: Lens' RtmpGroupSettings (Maybe RtmpCaptionData)
rgsCaptionData = lens _rgsCaptionData (\ s a -> s{_rgsCaptionData = a})

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
rgsRestartDelay :: Lens' RtmpGroupSettings (Maybe Natural)
rgsRestartDelay = lens _rgsRestartDelay (\ s a -> s{_rgsRestartDelay = a}) . mapping _Nat

-- | Authentication scheme to use when connecting with CDN
rgsAuthenticationScheme :: Lens' RtmpGroupSettings (Maybe AuthenticationScheme)
rgsAuthenticationScheme = lens _rgsAuthenticationScheme (\ s a -> s{_rgsAuthenticationScheme = a})

-- | Cache length, in seconds, is used to calculate buffer size.
rgsCacheLength :: Lens' RtmpGroupSettings (Maybe Natural)
rgsCacheLength = lens _rgsCacheLength (\ s a -> s{_rgsCacheLength = a}) . mapping _Nat

-- | Controls behavior when content cache fills up. If remote origin server stalls the RTMP connection and does not accept content fast enough the 'Media Cache' will fill up. When the cache reaches the duration specified by cacheLength the cache will stop accepting new content. If set to disconnectImmediately, the RTMP output will force a disconnect. Clear the media cache, and reconnect after restartDelay seconds. If set to waitForServer, the RTMP output will wait up to 5 minutes to allow the origin server to begin accepting data again.
rgsCacheFullBehavior :: Lens' RtmpGroupSettings (Maybe RtmpCacheFullBehavior)
rgsCacheFullBehavior = lens _rgsCacheFullBehavior (\ s a -> s{_rgsCacheFullBehavior = a})

instance FromJSON RtmpGroupSettings where
        parseJSON
          = withObject "RtmpGroupSettings"
              (\ x ->
                 RtmpGroupSettings' <$>
                   (x .:? "inputLossAction") <*> (x .:? "captionData")
                     <*> (x .:? "restartDelay")
                     <*> (x .:? "authenticationScheme")
                     <*> (x .:? "cacheLength")
                     <*> (x .:? "cacheFullBehavior"))

instance Hashable RtmpGroupSettings where

instance NFData RtmpGroupSettings where

instance ToJSON RtmpGroupSettings where
        toJSON RtmpGroupSettings'{..}
          = object
              (catMaybes
                 [("inputLossAction" .=) <$> _rgsInputLossAction,
                  ("captionData" .=) <$> _rgsCaptionData,
                  ("restartDelay" .=) <$> _rgsRestartDelay,
                  ("authenticationScheme" .=) <$>
                    _rgsAuthenticationScheme,
                  ("cacheLength" .=) <$> _rgsCacheLength,
                  ("cacheFullBehavior" .=) <$> _rgsCacheFullBehavior])

-- | Rtmp Output Settings
--
-- /See:/ 'rtmpOutputSettings' smart constructor.
data RtmpOutputSettings = RtmpOutputSettings'{_rosNumRetries
                                              :: !(Maybe Nat),
                                              _rosCertificateMode ::
                                              !(Maybe
                                                  RtmpOutputCertificateMode),
                                              _rosConnectionRetryInterval ::
                                              !(Maybe Nat),
                                              _rosDestination ::
                                              !OutputLocationRef}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RtmpOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rosNumRetries' - Number of retry attempts.
--
-- * 'rosCertificateMode' - If set to verifyAuthenticity, verify the tls certificate chain to a trusted Certificate Authority (CA).  This will cause rtmps outputs with self-signed certificates to fail.
--
-- * 'rosConnectionRetryInterval' - Number of seconds to wait before retrying a connection to the Flash Media server if the connection is lost.
--
-- * 'rosDestination' - The RTMP endpoint excluding the stream name (eg. rtmp://host/appname). For connection to Akamai, a username and password must be supplied. URI fields accept format identifiers.
rtmpOutputSettings
    :: OutputLocationRef -- ^ 'rosDestination'
    -> RtmpOutputSettings
rtmpOutputSettings pDestination_
  = RtmpOutputSettings'{_rosNumRetries = Nothing,
                        _rosCertificateMode = Nothing,
                        _rosConnectionRetryInterval = Nothing,
                        _rosDestination = pDestination_}

-- | Number of retry attempts.
rosNumRetries :: Lens' RtmpOutputSettings (Maybe Natural)
rosNumRetries = lens _rosNumRetries (\ s a -> s{_rosNumRetries = a}) . mapping _Nat

-- | If set to verifyAuthenticity, verify the tls certificate chain to a trusted Certificate Authority (CA).  This will cause rtmps outputs with self-signed certificates to fail.
rosCertificateMode :: Lens' RtmpOutputSettings (Maybe RtmpOutputCertificateMode)
rosCertificateMode = lens _rosCertificateMode (\ s a -> s{_rosCertificateMode = a})

-- | Number of seconds to wait before retrying a connection to the Flash Media server if the connection is lost.
rosConnectionRetryInterval :: Lens' RtmpOutputSettings (Maybe Natural)
rosConnectionRetryInterval = lens _rosConnectionRetryInterval (\ s a -> s{_rosConnectionRetryInterval = a}) . mapping _Nat

-- | The RTMP endpoint excluding the stream name (eg. rtmp://host/appname). For connection to Akamai, a username and password must be supplied. URI fields accept format identifiers.
rosDestination :: Lens' RtmpOutputSettings OutputLocationRef
rosDestination = lens _rosDestination (\ s a -> s{_rosDestination = a})

instance FromJSON RtmpOutputSettings where
        parseJSON
          = withObject "RtmpOutputSettings"
              (\ x ->
                 RtmpOutputSettings' <$>
                   (x .:? "numRetries") <*> (x .:? "certificateMode")
                     <*> (x .:? "connectionRetryInterval")
                     <*> (x .: "destination"))

instance Hashable RtmpOutputSettings where

instance NFData RtmpOutputSettings where

instance ToJSON RtmpOutputSettings where
        toJSON RtmpOutputSettings'{..}
          = object
              (catMaybes
                 [("numRetries" .=) <$> _rosNumRetries,
                  ("certificateMode" .=) <$> _rosCertificateMode,
                  ("connectionRetryInterval" .=) <$>
                    _rosConnectionRetryInterval,
                  Just ("destination" .= _rosDestination)])

-- | Contains information on a single schedule action.
--
-- /See:/ 'scheduleAction' smart constructor.
data ScheduleAction = ScheduleAction'{_saActionName
                                      :: !Text,
                                      _saScheduleActionStartSettings ::
                                      !ScheduleActionStartSettings,
                                      _saScheduleActionSettings ::
                                      !ScheduleActionSettings}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduleAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saActionName' - The name of the action, must be unique within the schedule. This name provides the main reference to an action once it is added to the schedule. A name is unique if it is no longer in the schedule. The schedule is automatically cleaned up to remove actions with a start time of more than 1 hour ago (approximately) so at that point a name can be reused.
--
-- * 'saScheduleActionStartSettings' - The time for the action to start in the channel.
--
-- * 'saScheduleActionSettings' - Settings for this schedule action.
scheduleAction
    :: Text -- ^ 'saActionName'
    -> ScheduleActionStartSettings -- ^ 'saScheduleActionStartSettings'
    -> ScheduleActionSettings -- ^ 'saScheduleActionSettings'
    -> ScheduleAction
scheduleAction pActionName_
  pScheduleActionStartSettings_
  pScheduleActionSettings_
  = ScheduleAction'{_saActionName = pActionName_,
                    _saScheduleActionStartSettings =
                      pScheduleActionStartSettings_,
                    _saScheduleActionSettings = pScheduleActionSettings_}

-- | The name of the action, must be unique within the schedule. This name provides the main reference to an action once it is added to the schedule. A name is unique if it is no longer in the schedule. The schedule is automatically cleaned up to remove actions with a start time of more than 1 hour ago (approximately) so at that point a name can be reused.
saActionName :: Lens' ScheduleAction Text
saActionName = lens _saActionName (\ s a -> s{_saActionName = a})

-- | The time for the action to start in the channel.
saScheduleActionStartSettings :: Lens' ScheduleAction ScheduleActionStartSettings
saScheduleActionStartSettings = lens _saScheduleActionStartSettings (\ s a -> s{_saScheduleActionStartSettings = a})

-- | Settings for this schedule action.
saScheduleActionSettings :: Lens' ScheduleAction ScheduleActionSettings
saScheduleActionSettings = lens _saScheduleActionSettings (\ s a -> s{_saScheduleActionSettings = a})

instance FromJSON ScheduleAction where
        parseJSON
          = withObject "ScheduleAction"
              (\ x ->
                 ScheduleAction' <$>
                   (x .: "actionName") <*>
                     (x .: "scheduleActionStartSettings")
                     <*> (x .: "scheduleActionSettings"))

instance Hashable ScheduleAction where

instance NFData ScheduleAction where

instance ToJSON ScheduleAction where
        toJSON ScheduleAction'{..}
          = object
              (catMaybes
                 [Just ("actionName" .= _saActionName),
                  Just
                    ("scheduleActionStartSettings" .=
                       _saScheduleActionStartSettings),
                  Just
                    ("scheduleActionSettings" .=
                       _saScheduleActionSettings)])

-- | Holds the settings for a single schedule action.
--
-- /See:/ 'scheduleActionSettings' smart constructor.
data ScheduleActionSettings = ScheduleActionSettings'{_sasStaticImageDeactivateSettings
                                                      ::
                                                      !(Maybe
                                                          StaticImageDeactivateScheduleActionSettings),
                                                      _sasScte35SpliceInsertSettings
                                                      ::
                                                      !(Maybe
                                                          Scte35SpliceInsertScheduleActionSettings),
                                                      _sasStaticImageActivateSettings
                                                      ::
                                                      !(Maybe
                                                          StaticImageActivateScheduleActionSettings),
                                                      _sasScte35TimeSignalSettings
                                                      ::
                                                      !(Maybe
                                                          Scte35TimeSignalScheduleActionSettings),
                                                      _sasInputPrepareSettings
                                                      ::
                                                      !(Maybe
                                                          InputPrepareScheduleActionSettings),
                                                      _sasHlsId3SegmentTaggingSettings
                                                      ::
                                                      !(Maybe
                                                          HlsId3SegmentTaggingScheduleActionSettings),
                                                      _sasScte35ReturnToNetworkSettings
                                                      ::
                                                      !(Maybe
                                                          Scte35ReturnToNetworkScheduleActionSettings),
                                                      _sasPauseStateSettings ::
                                                      !(Maybe
                                                          PauseStateScheduleActionSettings),
                                                      _sasHlsTimedMetadataSettings
                                                      ::
                                                      !(Maybe
                                                          HlsTimedMetadataScheduleActionSettings),
                                                      _sasInputSwitchSettings ::
                                                      !(Maybe
                                                          InputSwitchScheduleActionSettings)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'ScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sasStaticImageDeactivateSettings' - Action to deactivate a static image overlay
--
-- * 'sasScte35SpliceInsertSettings' - Action to insert SCTE-35 splice_insert message
--
-- * 'sasStaticImageActivateSettings' - Action to activate a static image overlay
--
-- * 'sasScte35TimeSignalSettings' - Action to insert SCTE-35 time_signal message
--
-- * 'sasInputPrepareSettings' - Action to prepare an input for a future immediate input switch
--
-- * 'sasHlsId3SegmentTaggingSettings' - Action to insert HLS ID3 segment tagging
--
-- * 'sasScte35ReturnToNetworkSettings' - Action to insert SCTE-35 return_to_network message
--
-- * 'sasPauseStateSettings' - Action to pause or unpause one or both channel pipelines
--
-- * 'sasHlsTimedMetadataSettings' - Action to insert HLS metadata
--
-- * 'sasInputSwitchSettings' - Action to switch the input
scheduleActionSettings
    :: ScheduleActionSettings
scheduleActionSettings
  = ScheduleActionSettings'{_sasStaticImageDeactivateSettings
                              = Nothing,
                            _sasScte35SpliceInsertSettings = Nothing,
                            _sasStaticImageActivateSettings = Nothing,
                            _sasScte35TimeSignalSettings = Nothing,
                            _sasInputPrepareSettings = Nothing,
                            _sasHlsId3SegmentTaggingSettings = Nothing,
                            _sasScte35ReturnToNetworkSettings = Nothing,
                            _sasPauseStateSettings = Nothing,
                            _sasHlsTimedMetadataSettings = Nothing,
                            _sasInputSwitchSettings = Nothing}

-- | Action to deactivate a static image overlay
sasStaticImageDeactivateSettings :: Lens' ScheduleActionSettings (Maybe StaticImageDeactivateScheduleActionSettings)
sasStaticImageDeactivateSettings = lens _sasStaticImageDeactivateSettings (\ s a -> s{_sasStaticImageDeactivateSettings = a})

-- | Action to insert SCTE-35 splice_insert message
sasScte35SpliceInsertSettings :: Lens' ScheduleActionSettings (Maybe Scte35SpliceInsertScheduleActionSettings)
sasScte35SpliceInsertSettings = lens _sasScte35SpliceInsertSettings (\ s a -> s{_sasScte35SpliceInsertSettings = a})

-- | Action to activate a static image overlay
sasStaticImageActivateSettings :: Lens' ScheduleActionSettings (Maybe StaticImageActivateScheduleActionSettings)
sasStaticImageActivateSettings = lens _sasStaticImageActivateSettings (\ s a -> s{_sasStaticImageActivateSettings = a})

-- | Action to insert SCTE-35 time_signal message
sasScte35TimeSignalSettings :: Lens' ScheduleActionSettings (Maybe Scte35TimeSignalScheduleActionSettings)
sasScte35TimeSignalSettings = lens _sasScte35TimeSignalSettings (\ s a -> s{_sasScte35TimeSignalSettings = a})

-- | Action to prepare an input for a future immediate input switch
sasInputPrepareSettings :: Lens' ScheduleActionSettings (Maybe InputPrepareScheduleActionSettings)
sasInputPrepareSettings = lens _sasInputPrepareSettings (\ s a -> s{_sasInputPrepareSettings = a})

-- | Action to insert HLS ID3 segment tagging
sasHlsId3SegmentTaggingSettings :: Lens' ScheduleActionSettings (Maybe HlsId3SegmentTaggingScheduleActionSettings)
sasHlsId3SegmentTaggingSettings = lens _sasHlsId3SegmentTaggingSettings (\ s a -> s{_sasHlsId3SegmentTaggingSettings = a})

-- | Action to insert SCTE-35 return_to_network message
sasScte35ReturnToNetworkSettings :: Lens' ScheduleActionSettings (Maybe Scte35ReturnToNetworkScheduleActionSettings)
sasScte35ReturnToNetworkSettings = lens _sasScte35ReturnToNetworkSettings (\ s a -> s{_sasScte35ReturnToNetworkSettings = a})

-- | Action to pause or unpause one or both channel pipelines
sasPauseStateSettings :: Lens' ScheduleActionSettings (Maybe PauseStateScheduleActionSettings)
sasPauseStateSettings = lens _sasPauseStateSettings (\ s a -> s{_sasPauseStateSettings = a})

-- | Action to insert HLS metadata
sasHlsTimedMetadataSettings :: Lens' ScheduleActionSettings (Maybe HlsTimedMetadataScheduleActionSettings)
sasHlsTimedMetadataSettings = lens _sasHlsTimedMetadataSettings (\ s a -> s{_sasHlsTimedMetadataSettings = a})

-- | Action to switch the input
sasInputSwitchSettings :: Lens' ScheduleActionSettings (Maybe InputSwitchScheduleActionSettings)
sasInputSwitchSettings = lens _sasInputSwitchSettings (\ s a -> s{_sasInputSwitchSettings = a})

instance FromJSON ScheduleActionSettings where
        parseJSON
          = withObject "ScheduleActionSettings"
              (\ x ->
                 ScheduleActionSettings' <$>
                   (x .:? "staticImageDeactivateSettings") <*>
                     (x .:? "scte35SpliceInsertSettings")
                     <*> (x .:? "staticImageActivateSettings")
                     <*> (x .:? "scte35TimeSignalSettings")
                     <*> (x .:? "inputPrepareSettings")
                     <*> (x .:? "hlsId3SegmentTaggingSettings")
                     <*> (x .:? "scte35ReturnToNetworkSettings")
                     <*> (x .:? "pauseStateSettings")
                     <*> (x .:? "hlsTimedMetadataSettings")
                     <*> (x .:? "inputSwitchSettings"))

instance Hashable ScheduleActionSettings where

instance NFData ScheduleActionSettings where

instance ToJSON ScheduleActionSettings where
        toJSON ScheduleActionSettings'{..}
          = object
              (catMaybes
                 [("staticImageDeactivateSettings" .=) <$>
                    _sasStaticImageDeactivateSettings,
                  ("scte35SpliceInsertSettings" .=) <$>
                    _sasScte35SpliceInsertSettings,
                  ("staticImageActivateSettings" .=) <$>
                    _sasStaticImageActivateSettings,
                  ("scte35TimeSignalSettings" .=) <$>
                    _sasScte35TimeSignalSettings,
                  ("inputPrepareSettings" .=) <$>
                    _sasInputPrepareSettings,
                  ("hlsId3SegmentTaggingSettings" .=) <$>
                    _sasHlsId3SegmentTaggingSettings,
                  ("scte35ReturnToNetworkSettings" .=) <$>
                    _sasScte35ReturnToNetworkSettings,
                  ("pauseStateSettings" .=) <$> _sasPauseStateSettings,
                  ("hlsTimedMetadataSettings" .=) <$>
                    _sasHlsTimedMetadataSettings,
                  ("inputSwitchSettings" .=) <$>
                    _sasInputSwitchSettings])

-- | Settings to specify when an action should occur. Only one of the options must be selected.
--
-- /See:/ 'scheduleActionStartSettings' smart constructor.
data ScheduleActionStartSettings = ScheduleActionStartSettings'{_sassImmediateModeScheduleActionStartSettings
                                                                ::
                                                                !(Maybe
                                                                    ImmediateModeScheduleActionStartSettings),
                                                                _sassFollowModeScheduleActionStartSettings
                                                                ::
                                                                !(Maybe
                                                                    FollowModeScheduleActionStartSettings),
                                                                _sassFixedModeScheduleActionStartSettings
                                                                ::
                                                                !(Maybe
                                                                    FixedModeScheduleActionStartSettings)}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'ScheduleActionStartSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sassImmediateModeScheduleActionStartSettings' - Option for specifying an action that should be applied immediately.
--
-- * 'sassFollowModeScheduleActionStartSettings' - Option for specifying an action as relative to another action.
--
-- * 'sassFixedModeScheduleActionStartSettings' - Option for specifying the start time for an action.
scheduleActionStartSettings
    :: ScheduleActionStartSettings
scheduleActionStartSettings
  = ScheduleActionStartSettings'{_sassImmediateModeScheduleActionStartSettings
                                   = Nothing,
                                 _sassFollowModeScheduleActionStartSettings =
                                   Nothing,
                                 _sassFixedModeScheduleActionStartSettings =
                                   Nothing}

-- | Option for specifying an action that should be applied immediately.
sassImmediateModeScheduleActionStartSettings :: Lens' ScheduleActionStartSettings (Maybe ImmediateModeScheduleActionStartSettings)
sassImmediateModeScheduleActionStartSettings = lens _sassImmediateModeScheduleActionStartSettings (\ s a -> s{_sassImmediateModeScheduleActionStartSettings = a})

-- | Option for specifying an action as relative to another action.
sassFollowModeScheduleActionStartSettings :: Lens' ScheduleActionStartSettings (Maybe FollowModeScheduleActionStartSettings)
sassFollowModeScheduleActionStartSettings = lens _sassFollowModeScheduleActionStartSettings (\ s a -> s{_sassFollowModeScheduleActionStartSettings = a})

-- | Option for specifying the start time for an action.
sassFixedModeScheduleActionStartSettings :: Lens' ScheduleActionStartSettings (Maybe FixedModeScheduleActionStartSettings)
sassFixedModeScheduleActionStartSettings = lens _sassFixedModeScheduleActionStartSettings (\ s a -> s{_sassFixedModeScheduleActionStartSettings = a})

instance FromJSON ScheduleActionStartSettings where
        parseJSON
          = withObject "ScheduleActionStartSettings"
              (\ x ->
                 ScheduleActionStartSettings' <$>
                   (x .:? "immediateModeScheduleActionStartSettings")
                     <*> (x .:? "followModeScheduleActionStartSettings")
                     <*> (x .:? "fixedModeScheduleActionStartSettings"))

instance Hashable ScheduleActionStartSettings where

instance NFData ScheduleActionStartSettings where

instance ToJSON ScheduleActionStartSettings where
        toJSON ScheduleActionStartSettings'{..}
          = object
              (catMaybes
                 [("immediateModeScheduleActionStartSettings" .=) <$>
                    _sassImmediateModeScheduleActionStartSettings,
                  ("followModeScheduleActionStartSettings" .=) <$>
                    _sassFollowModeScheduleActionStartSettings,
                  ("fixedModeScheduleActionStartSettings" .=) <$>
                    _sassFixedModeScheduleActionStartSettings])

-- | Scte20 Plus Embedded Destination Settings
--
-- /See:/ 'scte20PlusEmbeddedDestinationSettings' smart constructor.
data Scte20PlusEmbeddedDestinationSettings = Scte20PlusEmbeddedDestinationSettings'
                                               deriving (Eq, Read, Show, Data,
                                                         Typeable, Generic)

-- | Creates a value of 'Scte20PlusEmbeddedDestinationSettings' with the minimum fields required to make a request.
--
scte20PlusEmbeddedDestinationSettings
    :: Scte20PlusEmbeddedDestinationSettings
scte20PlusEmbeddedDestinationSettings
  = Scte20PlusEmbeddedDestinationSettings'

instance FromJSON
           Scte20PlusEmbeddedDestinationSettings
         where
        parseJSON
          = withObject "Scte20PlusEmbeddedDestinationSettings"
              (\ x -> pure Scte20PlusEmbeddedDestinationSettings')

instance Hashable
           Scte20PlusEmbeddedDestinationSettings
         where

instance NFData Scte20PlusEmbeddedDestinationSettings
         where

instance ToJSON Scte20PlusEmbeddedDestinationSettings
         where
        toJSON = const (Object mempty)

-- | Scte20 Source Settings
--
-- /See:/ 'scte20SourceSettings' smart constructor.
data Scte20SourceSettings = Scte20SourceSettings'{_sssConvert608To708
                                                  ::
                                                  !(Maybe
                                                      Scte20Convert608To708),
                                                  _sssSource608ChannelNumber ::
                                                  !(Maybe Nat)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte20SourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssConvert608To708' - If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
--
-- * 'sssSource608ChannelNumber' - Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
scte20SourceSettings
    :: Scte20SourceSettings
scte20SourceSettings
  = Scte20SourceSettings'{_sssConvert608To708 =
                            Nothing,
                          _sssSource608ChannelNumber = Nothing}

-- | If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
sssConvert608To708 :: Lens' Scte20SourceSettings (Maybe Scte20Convert608To708)
sssConvert608To708 = lens _sssConvert608To708 (\ s a -> s{_sssConvert608To708 = a})

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
sssSource608ChannelNumber :: Lens' Scte20SourceSettings (Maybe Natural)
sssSource608ChannelNumber = lens _sssSource608ChannelNumber (\ s a -> s{_sssSource608ChannelNumber = a}) . mapping _Nat

instance FromJSON Scte20SourceSettings where
        parseJSON
          = withObject "Scte20SourceSettings"
              (\ x ->
                 Scte20SourceSettings' <$>
                   (x .:? "convert608To708") <*>
                     (x .:? "source608ChannelNumber"))

instance Hashable Scte20SourceSettings where

instance NFData Scte20SourceSettings where

instance ToJSON Scte20SourceSettings where
        toJSON Scte20SourceSettings'{..}
          = object
              (catMaybes
                 [("convert608To708" .=) <$> _sssConvert608To708,
                  ("source608ChannelNumber" .=) <$>
                    _sssSource608ChannelNumber])

-- | Scte27 Destination Settings
--
-- /See:/ 'scte27DestinationSettings' smart constructor.
data Scte27DestinationSettings = Scte27DestinationSettings'
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'Scte27DestinationSettings' with the minimum fields required to make a request.
--
scte27DestinationSettings
    :: Scte27DestinationSettings
scte27DestinationSettings
  = Scte27DestinationSettings'

instance FromJSON Scte27DestinationSettings where
        parseJSON
          = withObject "Scte27DestinationSettings"
              (\ x -> pure Scte27DestinationSettings')

instance Hashable Scte27DestinationSettings where

instance NFData Scte27DestinationSettings where

instance ToJSON Scte27DestinationSettings where
        toJSON = const (Object mempty)

-- | Scte27 Source Settings
--
-- /See:/ 'scte27SourceSettings' smart constructor.
newtype Scte27SourceSettings = Scte27SourceSettings'{_sssPid
                                                     :: Maybe Nat}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'Scte27SourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssPid' - The pid field is used in conjunction with the caption selector languageCode field as follows:   - Specify PID and Language: Extracts captions from that PID; the language is "informational".   - Specify PID and omit Language: Extracts the specified PID.   - Omit PID and specify Language: Extracts the specified language, whichever PID that happens to be.   - Omit PID and omit Language: Valid only if source is DVB-Sub that is being passed through; all languages will be passed through.
scte27SourceSettings
    :: Scte27SourceSettings
scte27SourceSettings
  = Scte27SourceSettings'{_sssPid = Nothing}

-- | The pid field is used in conjunction with the caption selector languageCode field as follows:   - Specify PID and Language: Extracts captions from that PID; the language is "informational".   - Specify PID and omit Language: Extracts the specified PID.   - Omit PID and specify Language: Extracts the specified language, whichever PID that happens to be.   - Omit PID and omit Language: Valid only if source is DVB-Sub that is being passed through; all languages will be passed through.
sssPid :: Lens' Scte27SourceSettings (Maybe Natural)
sssPid = lens _sssPid (\ s a -> s{_sssPid = a}) . mapping _Nat

instance FromJSON Scte27SourceSettings where
        parseJSON
          = withObject "Scte27SourceSettings"
              (\ x -> Scte27SourceSettings' <$> (x .:? "pid"))

instance Hashable Scte27SourceSettings where

instance NFData Scte27SourceSettings where

instance ToJSON Scte27SourceSettings where
        toJSON Scte27SourceSettings'{..}
          = object (catMaybes [("pid" .=) <$> _sssPid])

-- | Corresponds to SCTE-35 delivery_not_restricted_flag parameter. To declare delivery restrictions, include this element and its four "restriction" flags. To declare that there are no restrictions, omit this element.
--
-- /See:/ 'scte35DeliveryRestrictions' smart constructor.
data Scte35DeliveryRestrictions = Scte35DeliveryRestrictions'{_sdrDeviceRestrictions
                                                              ::
                                                              !Scte35DeviceRestrictions,
                                                              _sdrArchiveAllowedFlag
                                                              ::
                                                              !Scte35ArchiveAllowedFlag,
                                                              _sdrWebDeliveryAllowedFlag
                                                              ::
                                                              !Scte35WebDeliveryAllowedFlag,
                                                              _sdrNoRegionalBlackoutFlag
                                                              ::
                                                              !Scte35NoRegionalBlackoutFlag}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'Scte35DeliveryRestrictions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdrDeviceRestrictions' - Corresponds to SCTE-35 device_restrictions parameter.
--
-- * 'sdrArchiveAllowedFlag' - Corresponds to SCTE-35 archive_allowed_flag.
--
-- * 'sdrWebDeliveryAllowedFlag' - Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
--
-- * 'sdrNoRegionalBlackoutFlag' - Corresponds to SCTE-35 no_regional_blackout_flag parameter.
scte35DeliveryRestrictions
    :: Scte35DeviceRestrictions -- ^ 'sdrDeviceRestrictions'
    -> Scte35ArchiveAllowedFlag -- ^ 'sdrArchiveAllowedFlag'
    -> Scte35WebDeliveryAllowedFlag -- ^ 'sdrWebDeliveryAllowedFlag'
    -> Scte35NoRegionalBlackoutFlag -- ^ 'sdrNoRegionalBlackoutFlag'
    -> Scte35DeliveryRestrictions
scte35DeliveryRestrictions pDeviceRestrictions_
  pArchiveAllowedFlag_ pWebDeliveryAllowedFlag_
  pNoRegionalBlackoutFlag_
  = Scte35DeliveryRestrictions'{_sdrDeviceRestrictions
                                  = pDeviceRestrictions_,
                                _sdrArchiveAllowedFlag = pArchiveAllowedFlag_,
                                _sdrWebDeliveryAllowedFlag =
                                  pWebDeliveryAllowedFlag_,
                                _sdrNoRegionalBlackoutFlag =
                                  pNoRegionalBlackoutFlag_}

-- | Corresponds to SCTE-35 device_restrictions parameter.
sdrDeviceRestrictions :: Lens' Scte35DeliveryRestrictions Scte35DeviceRestrictions
sdrDeviceRestrictions = lens _sdrDeviceRestrictions (\ s a -> s{_sdrDeviceRestrictions = a})

-- | Corresponds to SCTE-35 archive_allowed_flag.
sdrArchiveAllowedFlag :: Lens' Scte35DeliveryRestrictions Scte35ArchiveAllowedFlag
sdrArchiveAllowedFlag = lens _sdrArchiveAllowedFlag (\ s a -> s{_sdrArchiveAllowedFlag = a})

-- | Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
sdrWebDeliveryAllowedFlag :: Lens' Scte35DeliveryRestrictions Scte35WebDeliveryAllowedFlag
sdrWebDeliveryAllowedFlag = lens _sdrWebDeliveryAllowedFlag (\ s a -> s{_sdrWebDeliveryAllowedFlag = a})

-- | Corresponds to SCTE-35 no_regional_blackout_flag parameter.
sdrNoRegionalBlackoutFlag :: Lens' Scte35DeliveryRestrictions Scte35NoRegionalBlackoutFlag
sdrNoRegionalBlackoutFlag = lens _sdrNoRegionalBlackoutFlag (\ s a -> s{_sdrNoRegionalBlackoutFlag = a})

instance FromJSON Scte35DeliveryRestrictions where
        parseJSON
          = withObject "Scte35DeliveryRestrictions"
              (\ x ->
                 Scte35DeliveryRestrictions' <$>
                   (x .: "deviceRestrictions") <*>
                     (x .: "archiveAllowedFlag")
                     <*> (x .: "webDeliveryAllowedFlag")
                     <*> (x .: "noRegionalBlackoutFlag"))

instance Hashable Scte35DeliveryRestrictions where

instance NFData Scte35DeliveryRestrictions where

instance ToJSON Scte35DeliveryRestrictions where
        toJSON Scte35DeliveryRestrictions'{..}
          = object
              (catMaybes
                 [Just
                    ("deviceRestrictions" .= _sdrDeviceRestrictions),
                  Just
                    ("archiveAllowedFlag" .= _sdrArchiveAllowedFlag),
                  Just
                    ("webDeliveryAllowedFlag" .=
                       _sdrWebDeliveryAllowedFlag),
                  Just
                    ("noRegionalBlackoutFlag" .=
                       _sdrNoRegionalBlackoutFlag)])

-- | Holds one set of SCTE-35 Descriptor Settings.
--
-- /See:/ 'scte35Descriptor' smart constructor.
newtype Scte35Descriptor = Scte35Descriptor'{_sdScte35DescriptorSettings
                                             :: Scte35DescriptorSettings}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte35Descriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdScte35DescriptorSettings' - SCTE-35 Descriptor Settings.
scte35Descriptor
    :: Scte35DescriptorSettings -- ^ 'sdScte35DescriptorSettings'
    -> Scte35Descriptor
scte35Descriptor pScte35DescriptorSettings_
  = Scte35Descriptor'{_sdScte35DescriptorSettings =
                        pScte35DescriptorSettings_}

-- | SCTE-35 Descriptor Settings.
sdScte35DescriptorSettings :: Lens' Scte35Descriptor Scte35DescriptorSettings
sdScte35DescriptorSettings = lens _sdScte35DescriptorSettings (\ s a -> s{_sdScte35DescriptorSettings = a})

instance FromJSON Scte35Descriptor where
        parseJSON
          = withObject "Scte35Descriptor"
              (\ x ->
                 Scte35Descriptor' <$>
                   (x .: "scte35DescriptorSettings"))

instance Hashable Scte35Descriptor where

instance NFData Scte35Descriptor where

instance ToJSON Scte35Descriptor where
        toJSON Scte35Descriptor'{..}
          = object
              (catMaybes
                 [Just
                    ("scte35DescriptorSettings" .=
                       _sdScte35DescriptorSettings)])

-- | SCTE-35 Descriptor settings.
--
-- /See:/ 'scte35DescriptorSettings' smart constructor.
newtype Scte35DescriptorSettings = Scte35DescriptorSettings'{_sdsSegmentationDescriptorScte35DescriptorSettings
                                                             ::
                                                             Scte35SegmentationDescriptor}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'Scte35DescriptorSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsSegmentationDescriptorScte35DescriptorSettings' - SCTE-35 Segmentation Descriptor.
scte35DescriptorSettings
    :: Scte35SegmentationDescriptor -- ^ 'sdsSegmentationDescriptorScte35DescriptorSettings'
    -> Scte35DescriptorSettings
scte35DescriptorSettings
  pSegmentationDescriptorScte35DescriptorSettings_
  = Scte35DescriptorSettings'{_sdsSegmentationDescriptorScte35DescriptorSettings
                                =
                                pSegmentationDescriptorScte35DescriptorSettings_}

-- | SCTE-35 Segmentation Descriptor.
sdsSegmentationDescriptorScte35DescriptorSettings :: Lens' Scte35DescriptorSettings Scte35SegmentationDescriptor
sdsSegmentationDescriptorScte35DescriptorSettings = lens _sdsSegmentationDescriptorScte35DescriptorSettings (\ s a -> s{_sdsSegmentationDescriptorScte35DescriptorSettings = a})

instance FromJSON Scte35DescriptorSettings where
        parseJSON
          = withObject "Scte35DescriptorSettings"
              (\ x ->
                 Scte35DescriptorSettings' <$>
                   (x .:
                      "segmentationDescriptorScte35DescriptorSettings"))

instance Hashable Scte35DescriptorSettings where

instance NFData Scte35DescriptorSettings where

instance ToJSON Scte35DescriptorSettings where
        toJSON Scte35DescriptorSettings'{..}
          = object
              (catMaybes
                 [Just
                    ("segmentationDescriptorScte35DescriptorSettings" .=
                       _sdsSegmentationDescriptorScte35DescriptorSettings)])

-- | Settings for a SCTE-35 return_to_network message.
--
-- /See:/ 'scte35ReturnToNetworkScheduleActionSettings' smart constructor.
newtype Scte35ReturnToNetworkScheduleActionSettings = Scte35ReturnToNetworkScheduleActionSettings'{_srtnsasSpliceEventId
                                                                                                   ::
                                                                                                   Nat}
                                                        deriving (Eq, Read,
                                                                  Show, Data,
                                                                  Typeable,
                                                                  Generic)

-- | Creates a value of 'Scte35ReturnToNetworkScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtnsasSpliceEventId' - The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
scte35ReturnToNetworkScheduleActionSettings
    :: Natural -- ^ 'srtnsasSpliceEventId'
    -> Scte35ReturnToNetworkScheduleActionSettings
scte35ReturnToNetworkScheduleActionSettings
  pSpliceEventId_
  = Scte35ReturnToNetworkScheduleActionSettings'{_srtnsasSpliceEventId
                                                   = _Nat # pSpliceEventId_}

-- | The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
srtnsasSpliceEventId :: Lens' Scte35ReturnToNetworkScheduleActionSettings Natural
srtnsasSpliceEventId = lens _srtnsasSpliceEventId (\ s a -> s{_srtnsasSpliceEventId = a}) . _Nat

instance FromJSON
           Scte35ReturnToNetworkScheduleActionSettings
         where
        parseJSON
          = withObject
              "Scte35ReturnToNetworkScheduleActionSettings"
              (\ x ->
                 Scte35ReturnToNetworkScheduleActionSettings' <$>
                   (x .: "spliceEventId"))

instance Hashable
           Scte35ReturnToNetworkScheduleActionSettings
         where

instance NFData
           Scte35ReturnToNetworkScheduleActionSettings
         where

instance ToJSON
           Scte35ReturnToNetworkScheduleActionSettings
         where
        toJSON
          Scte35ReturnToNetworkScheduleActionSettings'{..}
          = object
              (catMaybes
                 [Just ("spliceEventId" .= _srtnsasSpliceEventId)])

-- | Corresponds to SCTE-35 segmentation_descriptor.
--
-- /See:/ 'scte35SegmentationDescriptor' smart constructor.
data Scte35SegmentationDescriptor = Scte35SegmentationDescriptor'{_ssdSegmentationUpidType
                                                                  ::
                                                                  !(Maybe Nat),
                                                                  _ssdSegmentsExpected
                                                                  ::
                                                                  !(Maybe Nat),
                                                                  _ssdSubSegmentsExpected
                                                                  ::
                                                                  !(Maybe Nat),
                                                                  _ssdSegmentNum
                                                                  ::
                                                                  !(Maybe Nat),
                                                                  _ssdSegmentationDuration
                                                                  ::
                                                                  !(Maybe Nat),
                                                                  _ssdSegmentationTypeId
                                                                  ::
                                                                  !(Maybe Nat),
                                                                  _ssdDeliveryRestrictions
                                                                  ::
                                                                  !(Maybe
                                                                      Scte35DeliveryRestrictions),
                                                                  _ssdSegmentationUpid
                                                                  ::
                                                                  !(Maybe Text),
                                                                  _ssdSubSegmentNum
                                                                  ::
                                                                  !(Maybe Nat),
                                                                  _ssdSegmentationEventId
                                                                  :: !Nat,
                                                                  _ssdSegmentationCancelIndicator
                                                                  ::
                                                                  !Scte35SegmentationCancelIndicator}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'Scte35SegmentationDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdSegmentationUpidType' - Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one of the types listed in the SCTE-35 specification, converted to a decimal. For example, "0x0C" hex from the specification is "12" in decimal. In the CLI, API, or an SDK, enter one of the types listed in the SCTE-35 specification, in either hex (for example, "0x0C" ) or in decimal (for example, "12").
--
-- * 'ssdSegmentsExpected' - Corresponds to SCTE-35 segments_expected. A value that is valid for the specified segmentation_type_id.
--
-- * 'ssdSubSegmentsExpected' - Corresponds to SCTE-35 sub_segments_expected. A value that is valid for the specified segmentation_type_id.
--
-- * 'ssdSegmentNum' - Corresponds to SCTE-35 segment_num. A value that is valid for the specified segmentation_type_id.
--
-- * 'ssdSegmentationDuration' - Corresponds to SCTE-35 segmentation_duration. Optional. The duration for the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not enter a duration, the time_signal will continue until you insert a cancellation message.
--
-- * 'ssdSegmentationTypeId' - Corresponds to SCTE-35 segmentation_type_id. One of the segmentation_type_id values listed in the SCTE-35 specification. On the console, enter the ID in decimal (for example, "52"). In the CLI, API, or an SDK, enter the ID in hex (for example, "0x34") or decimal (for example, "52").
--
-- * 'ssdDeliveryRestrictions' - Holds the four SCTE-35 delivery restriction parameters.
--
-- * 'ssdSegmentationUpid' - Corresponds to SCTE-35 segmentation_upid. Enter a string containing the hexadecimal representation of the characters that make up the SCTE-35 segmentation_upid value. Must contain an even number of hex characters. Do not include spaces between each hex pair. For example, the ASCII "ADS Information" becomes hex "41445320496e666f726d6174696f6e.
--
-- * 'ssdSubSegmentNum' - Corresponds to SCTE-35 sub_segment_num. A value that is valid for the specified segmentation_type_id.
--
-- * 'ssdSegmentationEventId' - Corresponds to SCTE-35 segmentation_event_id. 
--
-- * 'ssdSegmentationCancelIndicator' - Corresponds to SCTE-35 segmentation_event_cancel_indicator.
scte35SegmentationDescriptor
    :: Natural -- ^ 'ssdSegmentationEventId'
    -> Scte35SegmentationCancelIndicator -- ^ 'ssdSegmentationCancelIndicator'
    -> Scte35SegmentationDescriptor
scte35SegmentationDescriptor pSegmentationEventId_
  pSegmentationCancelIndicator_
  = Scte35SegmentationDescriptor'{_ssdSegmentationUpidType
                                    = Nothing,
                                  _ssdSegmentsExpected = Nothing,
                                  _ssdSubSegmentsExpected = Nothing,
                                  _ssdSegmentNum = Nothing,
                                  _ssdSegmentationDuration = Nothing,
                                  _ssdSegmentationTypeId = Nothing,
                                  _ssdDeliveryRestrictions = Nothing,
                                  _ssdSegmentationUpid = Nothing,
                                  _ssdSubSegmentNum = Nothing,
                                  _ssdSegmentationEventId =
                                    _Nat # pSegmentationEventId_,
                                  _ssdSegmentationCancelIndicator =
                                    pSegmentationCancelIndicator_}

-- | Corresponds to SCTE-35 segmentation_upid_type. On the console, enter one of the types listed in the SCTE-35 specification, converted to a decimal. For example, "0x0C" hex from the specification is "12" in decimal. In the CLI, API, or an SDK, enter one of the types listed in the SCTE-35 specification, in either hex (for example, "0x0C" ) or in decimal (for example, "12").
ssdSegmentationUpidType :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSegmentationUpidType = lens _ssdSegmentationUpidType (\ s a -> s{_ssdSegmentationUpidType = a}) . mapping _Nat

-- | Corresponds to SCTE-35 segments_expected. A value that is valid for the specified segmentation_type_id.
ssdSegmentsExpected :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSegmentsExpected = lens _ssdSegmentsExpected (\ s a -> s{_ssdSegmentsExpected = a}) . mapping _Nat

-- | Corresponds to SCTE-35 sub_segments_expected. A value that is valid for the specified segmentation_type_id.
ssdSubSegmentsExpected :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSubSegmentsExpected = lens _ssdSubSegmentsExpected (\ s a -> s{_ssdSubSegmentsExpected = a}) . mapping _Nat

-- | Corresponds to SCTE-35 segment_num. A value that is valid for the specified segmentation_type_id.
ssdSegmentNum :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSegmentNum = lens _ssdSegmentNum (\ s a -> s{_ssdSegmentNum = a}) . mapping _Nat

-- | Corresponds to SCTE-35 segmentation_duration. Optional. The duration for the time_signal, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. Enter time in 90 KHz clock ticks. If you do not enter a duration, the time_signal will continue until you insert a cancellation message.
ssdSegmentationDuration :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSegmentationDuration = lens _ssdSegmentationDuration (\ s a -> s{_ssdSegmentationDuration = a}) . mapping _Nat

-- | Corresponds to SCTE-35 segmentation_type_id. One of the segmentation_type_id values listed in the SCTE-35 specification. On the console, enter the ID in decimal (for example, "52"). In the CLI, API, or an SDK, enter the ID in hex (for example, "0x34") or decimal (for example, "52").
ssdSegmentationTypeId :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSegmentationTypeId = lens _ssdSegmentationTypeId (\ s a -> s{_ssdSegmentationTypeId = a}) . mapping _Nat

-- | Holds the four SCTE-35 delivery restriction parameters.
ssdDeliveryRestrictions :: Lens' Scte35SegmentationDescriptor (Maybe Scte35DeliveryRestrictions)
ssdDeliveryRestrictions = lens _ssdDeliveryRestrictions (\ s a -> s{_ssdDeliveryRestrictions = a})

-- | Corresponds to SCTE-35 segmentation_upid. Enter a string containing the hexadecimal representation of the characters that make up the SCTE-35 segmentation_upid value. Must contain an even number of hex characters. Do not include spaces between each hex pair. For example, the ASCII "ADS Information" becomes hex "41445320496e666f726d6174696f6e.
ssdSegmentationUpid :: Lens' Scte35SegmentationDescriptor (Maybe Text)
ssdSegmentationUpid = lens _ssdSegmentationUpid (\ s a -> s{_ssdSegmentationUpid = a})

-- | Corresponds to SCTE-35 sub_segment_num. A value that is valid for the specified segmentation_type_id.
ssdSubSegmentNum :: Lens' Scte35SegmentationDescriptor (Maybe Natural)
ssdSubSegmentNum = lens _ssdSubSegmentNum (\ s a -> s{_ssdSubSegmentNum = a}) . mapping _Nat

-- | Corresponds to SCTE-35 segmentation_event_id. 
ssdSegmentationEventId :: Lens' Scte35SegmentationDescriptor Natural
ssdSegmentationEventId = lens _ssdSegmentationEventId (\ s a -> s{_ssdSegmentationEventId = a}) . _Nat

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
ssdSegmentationCancelIndicator :: Lens' Scte35SegmentationDescriptor Scte35SegmentationCancelIndicator
ssdSegmentationCancelIndicator = lens _ssdSegmentationCancelIndicator (\ s a -> s{_ssdSegmentationCancelIndicator = a})

instance FromJSON Scte35SegmentationDescriptor where
        parseJSON
          = withObject "Scte35SegmentationDescriptor"
              (\ x ->
                 Scte35SegmentationDescriptor' <$>
                   (x .:? "segmentationUpidType") <*>
                     (x .:? "segmentsExpected")
                     <*> (x .:? "subSegmentsExpected")
                     <*> (x .:? "segmentNum")
                     <*> (x .:? "segmentationDuration")
                     <*> (x .:? "segmentationTypeId")
                     <*> (x .:? "deliveryRestrictions")
                     <*> (x .:? "segmentationUpid")
                     <*> (x .:? "subSegmentNum")
                     <*> (x .: "segmentationEventId")
                     <*> (x .: "segmentationCancelIndicator"))

instance Hashable Scte35SegmentationDescriptor where

instance NFData Scte35SegmentationDescriptor where

instance ToJSON Scte35SegmentationDescriptor where
        toJSON Scte35SegmentationDescriptor'{..}
          = object
              (catMaybes
                 [("segmentationUpidType" .=) <$>
                    _ssdSegmentationUpidType,
                  ("segmentsExpected" .=) <$> _ssdSegmentsExpected,
                  ("subSegmentsExpected" .=) <$>
                    _ssdSubSegmentsExpected,
                  ("segmentNum" .=) <$> _ssdSegmentNum,
                  ("segmentationDuration" .=) <$>
                    _ssdSegmentationDuration,
                  ("segmentationTypeId" .=) <$> _ssdSegmentationTypeId,
                  ("deliveryRestrictions" .=) <$>
                    _ssdDeliveryRestrictions,
                  ("segmentationUpid" .=) <$> _ssdSegmentationUpid,
                  ("subSegmentNum" .=) <$> _ssdSubSegmentNum,
                  Just
                    ("segmentationEventId" .= _ssdSegmentationEventId),
                  Just
                    ("segmentationCancelIndicator" .=
                       _ssdSegmentationCancelIndicator)])

-- | Scte35 Splice Insert
--
-- /See:/ 'scte35SpliceInsert' smart constructor.
data Scte35SpliceInsert = Scte35SpliceInsert'{_ssiWebDeliveryAllowedFlag
                                              ::
                                              !(Maybe
                                                  Scte35SpliceInsertWebDeliveryAllowedBehavior),
                                              _ssiAdAvailOffset :: !(Maybe Int),
                                              _ssiNoRegionalBlackoutFlag ::
                                              !(Maybe
                                                  Scte35SpliceInsertNoRegionalBlackoutBehavior)}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte35SpliceInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssiWebDeliveryAllowedFlag' - When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- * 'ssiAdAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
--
-- * 'ssiNoRegionalBlackoutFlag' - When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
scte35SpliceInsert
    :: Scte35SpliceInsert
scte35SpliceInsert
  = Scte35SpliceInsert'{_ssiWebDeliveryAllowedFlag =
                          Nothing,
                        _ssiAdAvailOffset = Nothing,
                        _ssiNoRegionalBlackoutFlag = Nothing}

-- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
ssiWebDeliveryAllowedFlag :: Lens' Scte35SpliceInsert (Maybe Scte35SpliceInsertWebDeliveryAllowedBehavior)
ssiWebDeliveryAllowedFlag = lens _ssiWebDeliveryAllowedFlag (\ s a -> s{_ssiWebDeliveryAllowedFlag = a})

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
ssiAdAvailOffset :: Lens' Scte35SpliceInsert (Maybe Int)
ssiAdAvailOffset = lens _ssiAdAvailOffset (\ s a -> s{_ssiAdAvailOffset = a})

-- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
ssiNoRegionalBlackoutFlag :: Lens' Scte35SpliceInsert (Maybe Scte35SpliceInsertNoRegionalBlackoutBehavior)
ssiNoRegionalBlackoutFlag = lens _ssiNoRegionalBlackoutFlag (\ s a -> s{_ssiNoRegionalBlackoutFlag = a})

instance FromJSON Scte35SpliceInsert where
        parseJSON
          = withObject "Scte35SpliceInsert"
              (\ x ->
                 Scte35SpliceInsert' <$>
                   (x .:? "webDeliveryAllowedFlag") <*>
                     (x .:? "adAvailOffset")
                     <*> (x .:? "noRegionalBlackoutFlag"))

instance Hashable Scte35SpliceInsert where

instance NFData Scte35SpliceInsert where

instance ToJSON Scte35SpliceInsert where
        toJSON Scte35SpliceInsert'{..}
          = object
              (catMaybes
                 [("webDeliveryAllowedFlag" .=) <$>
                    _ssiWebDeliveryAllowedFlag,
                  ("adAvailOffset" .=) <$> _ssiAdAvailOffset,
                  ("noRegionalBlackoutFlag" .=) <$>
                    _ssiNoRegionalBlackoutFlag])

-- | Settings for a SCTE-35 splice_insert message.
--
-- /See:/ 'scte35SpliceInsertScheduleActionSettings' smart constructor.
data Scte35SpliceInsertScheduleActionSettings = Scte35SpliceInsertScheduleActionSettings'{_ssisasDuration
                                                                                          ::
                                                                                          !(Maybe
                                                                                              Nat),
                                                                                          _ssisasSpliceEventId
                                                                                          ::
                                                                                          !Nat}
                                                  deriving (Eq, Read, Show,
                                                            Data, Typeable,
                                                            Generic)

-- | Creates a value of 'Scte35SpliceInsertScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssisasDuration' - Optional, the duration for the splice_insert, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. If you enter a duration, there is an expectation that the downstream system can read the duration and cue in at that time. If you do not enter a duration, the splice_insert will continue indefinitely and there is an expectation that you will enter a return_to_network to end the splice_insert at the appropriate time.
--
-- * 'ssisasSpliceEventId' - The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
scte35SpliceInsertScheduleActionSettings
    :: Natural -- ^ 'ssisasSpliceEventId'
    -> Scte35SpliceInsertScheduleActionSettings
scte35SpliceInsertScheduleActionSettings
  pSpliceEventId_
  = Scte35SpliceInsertScheduleActionSettings'{_ssisasDuration
                                                = Nothing,
                                              _ssisasSpliceEventId =
                                                _Nat # pSpliceEventId_}

-- | Optional, the duration for the splice_insert, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. If you enter a duration, there is an expectation that the downstream system can read the duration and cue in at that time. If you do not enter a duration, the splice_insert will continue indefinitely and there is an expectation that you will enter a return_to_network to end the splice_insert at the appropriate time.
ssisasDuration :: Lens' Scte35SpliceInsertScheduleActionSettings (Maybe Natural)
ssisasDuration = lens _ssisasDuration (\ s a -> s{_ssisasDuration = a}) . mapping _Nat

-- | The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
ssisasSpliceEventId :: Lens' Scte35SpliceInsertScheduleActionSettings Natural
ssisasSpliceEventId = lens _ssisasSpliceEventId (\ s a -> s{_ssisasSpliceEventId = a}) . _Nat

instance FromJSON
           Scte35SpliceInsertScheduleActionSettings
         where
        parseJSON
          = withObject
              "Scte35SpliceInsertScheduleActionSettings"
              (\ x ->
                 Scte35SpliceInsertScheduleActionSettings' <$>
                   (x .:? "duration") <*> (x .: "spliceEventId"))

instance Hashable
           Scte35SpliceInsertScheduleActionSettings
         where

instance NFData
           Scte35SpliceInsertScheduleActionSettings
         where

instance ToJSON
           Scte35SpliceInsertScheduleActionSettings
         where
        toJSON Scte35SpliceInsertScheduleActionSettings'{..}
          = object
              (catMaybes
                 [("duration" .=) <$> _ssisasDuration,
                  Just ("spliceEventId" .= _ssisasSpliceEventId)])

-- | Scte35 Time Signal Apos
--
-- /See:/ 'scte35TimeSignalApos' smart constructor.
data Scte35TimeSignalApos = Scte35TimeSignalApos'{_stsaWebDeliveryAllowedFlag
                                                  ::
                                                  !(Maybe
                                                      Scte35AposWebDeliveryAllowedBehavior),
                                                  _stsaAdAvailOffset ::
                                                  !(Maybe Int),
                                                  _stsaNoRegionalBlackoutFlag ::
                                                  !(Maybe
                                                      Scte35AposNoRegionalBlackoutBehavior)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte35TimeSignalApos' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsaWebDeliveryAllowedFlag' - When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- * 'stsaAdAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
--
-- * 'stsaNoRegionalBlackoutFlag' - When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
scte35TimeSignalApos
    :: Scte35TimeSignalApos
scte35TimeSignalApos
  = Scte35TimeSignalApos'{_stsaWebDeliveryAllowedFlag =
                            Nothing,
                          _stsaAdAvailOffset = Nothing,
                          _stsaNoRegionalBlackoutFlag = Nothing}

-- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
stsaWebDeliveryAllowedFlag :: Lens' Scte35TimeSignalApos (Maybe Scte35AposWebDeliveryAllowedBehavior)
stsaWebDeliveryAllowedFlag = lens _stsaWebDeliveryAllowedFlag (\ s a -> s{_stsaWebDeliveryAllowedFlag = a})

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
stsaAdAvailOffset :: Lens' Scte35TimeSignalApos (Maybe Int)
stsaAdAvailOffset = lens _stsaAdAvailOffset (\ s a -> s{_stsaAdAvailOffset = a})

-- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
stsaNoRegionalBlackoutFlag :: Lens' Scte35TimeSignalApos (Maybe Scte35AposNoRegionalBlackoutBehavior)
stsaNoRegionalBlackoutFlag = lens _stsaNoRegionalBlackoutFlag (\ s a -> s{_stsaNoRegionalBlackoutFlag = a})

instance FromJSON Scte35TimeSignalApos where
        parseJSON
          = withObject "Scte35TimeSignalApos"
              (\ x ->
                 Scte35TimeSignalApos' <$>
                   (x .:? "webDeliveryAllowedFlag") <*>
                     (x .:? "adAvailOffset")
                     <*> (x .:? "noRegionalBlackoutFlag"))

instance Hashable Scte35TimeSignalApos where

instance NFData Scte35TimeSignalApos where

instance ToJSON Scte35TimeSignalApos where
        toJSON Scte35TimeSignalApos'{..}
          = object
              (catMaybes
                 [("webDeliveryAllowedFlag" .=) <$>
                    _stsaWebDeliveryAllowedFlag,
                  ("adAvailOffset" .=) <$> _stsaAdAvailOffset,
                  ("noRegionalBlackoutFlag" .=) <$>
                    _stsaNoRegionalBlackoutFlag])

-- | Settings for a SCTE-35 time_signal.
--
-- /See:/ 'scte35TimeSignalScheduleActionSettings' smart constructor.
newtype Scte35TimeSignalScheduleActionSettings = Scte35TimeSignalScheduleActionSettings'{_stssasScte35Descriptors
                                                                                         ::
                                                                                         [Scte35Descriptor]}
                                                   deriving (Eq, Read, Show,
                                                             Data, Typeable,
                                                             Generic)

-- | Creates a value of 'Scte35TimeSignalScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stssasScte35Descriptors' - The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
scte35TimeSignalScheduleActionSettings
    :: Scte35TimeSignalScheduleActionSettings
scte35TimeSignalScheduleActionSettings
  = Scte35TimeSignalScheduleActionSettings'{_stssasScte35Descriptors
                                              = mempty}

-- | The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
stssasScte35Descriptors :: Lens' Scte35TimeSignalScheduleActionSettings [Scte35Descriptor]
stssasScte35Descriptors = lens _stssasScte35Descriptors (\ s a -> s{_stssasScte35Descriptors = a}) . _Coerce

instance FromJSON
           Scte35TimeSignalScheduleActionSettings
         where
        parseJSON
          = withObject "Scte35TimeSignalScheduleActionSettings"
              (\ x ->
                 Scte35TimeSignalScheduleActionSettings' <$>
                   (x .:? "scte35Descriptors" .!= mempty))

instance Hashable
           Scte35TimeSignalScheduleActionSettings
         where

instance NFData
           Scte35TimeSignalScheduleActionSettings
         where

instance ToJSON
           Scte35TimeSignalScheduleActionSettings
         where
        toJSON Scte35TimeSignalScheduleActionSettings'{..}
          = object
              (catMaybes
                 [Just
                    ("scte35Descriptors" .= _stssasScte35Descriptors)])

-- | Smpte Tt Destination Settings
--
-- /See:/ 'smpteTtDestinationSettings' smart constructor.
data SmpteTtDestinationSettings = SmpteTtDestinationSettings'
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'SmpteTtDestinationSettings' with the minimum fields required to make a request.
--
smpteTtDestinationSettings
    :: SmpteTtDestinationSettings
smpteTtDestinationSettings
  = SmpteTtDestinationSettings'

instance FromJSON SmpteTtDestinationSettings where
        parseJSON
          = withObject "SmpteTtDestinationSettings"
              (\ x -> pure SmpteTtDestinationSettings')

instance Hashable SmpteTtDestinationSettings where

instance NFData SmpteTtDestinationSettings where

instance ToJSON SmpteTtDestinationSettings where
        toJSON = const (Object mempty)

-- | Standard Hls Settings
--
-- /See:/ 'standardHlsSettings' smart constructor.
data StandardHlsSettings = StandardHlsSettings'{_shsAudioRenditionSets
                                                :: !(Maybe Text),
                                                _shsM3u8Settings ::
                                                !M3u8Settings}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StandardHlsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'shsAudioRenditionSets' - List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- * 'shsM3u8Settings' - Undocumented member.
standardHlsSettings
    :: M3u8Settings -- ^ 'shsM3u8Settings'
    -> StandardHlsSettings
standardHlsSettings pM3u8Settings_
  = StandardHlsSettings'{_shsAudioRenditionSets =
                           Nothing,
                         _shsM3u8Settings = pM3u8Settings_}

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
shsAudioRenditionSets :: Lens' StandardHlsSettings (Maybe Text)
shsAudioRenditionSets = lens _shsAudioRenditionSets (\ s a -> s{_shsAudioRenditionSets = a})

-- | Undocumented member.
shsM3u8Settings :: Lens' StandardHlsSettings M3u8Settings
shsM3u8Settings = lens _shsM3u8Settings (\ s a -> s{_shsM3u8Settings = a})

instance FromJSON StandardHlsSettings where
        parseJSON
          = withObject "StandardHlsSettings"
              (\ x ->
                 StandardHlsSettings' <$>
                   (x .:? "audioRenditionSets") <*>
                     (x .: "m3u8Settings"))

instance Hashable StandardHlsSettings where

instance NFData StandardHlsSettings where

instance ToJSON StandardHlsSettings where
        toJSON StandardHlsSettings'{..}
          = object
              (catMaybes
                 [("audioRenditionSets" .=) <$>
                    _shsAudioRenditionSets,
                  Just ("m3u8Settings" .= _shsM3u8Settings)])

-- | Settings to identify the start of the clip.
--
-- /See:/ 'startTimecode' smart constructor.
newtype StartTimecode = StartTimecode'{_sTimecode ::
                                       Maybe Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTimecode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTimecode' - The timecode for the frame where you want to start the clip. Optional; if not specified, the clip starts at first frame in the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
startTimecode
    :: StartTimecode
startTimecode = StartTimecode'{_sTimecode = Nothing}

-- | The timecode for the frame where you want to start the clip. Optional; if not specified, the clip starts at first frame in the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
sTimecode :: Lens' StartTimecode (Maybe Text)
sTimecode = lens _sTimecode (\ s a -> s{_sTimecode = a})

instance FromJSON StartTimecode where
        parseJSON
          = withObject "StartTimecode"
              (\ x -> StartTimecode' <$> (x .:? "timecode"))

instance Hashable StartTimecode where

instance NFData StartTimecode where

instance ToJSON StartTimecode where
        toJSON StartTimecode'{..}
          = object (catMaybes [("timecode" .=) <$> _sTimecode])

-- | Settings for the action to activate a static image.
--
-- /See:/ 'staticImageActivateScheduleActionSettings' smart constructor.
data StaticImageActivateScheduleActionSettings = StaticImageActivateScheduleActionSettings'{_siasasImageX
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Nat),
                                                                                            _siasasHeight
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Nat),
                                                                                            _siasasFadeOut
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Nat),
                                                                                            _siasasWidth
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Nat),
                                                                                            _siasasOpacity
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Nat),
                                                                                            _siasasLayer
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Nat),
                                                                                            _siasasDuration
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Nat),
                                                                                            _siasasImageY
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Nat),
                                                                                            _siasasFadeIn
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Nat),
                                                                                            _siasasImage
                                                                                            ::
                                                                                            !InputLocation}
                                                   deriving (Eq, Read, Show,
                                                             Data, Typeable,
                                                             Generic)

-- | Creates a value of 'StaticImageActivateScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siasasImageX' - Placement of the left edge of the overlay relative to the left edge of the video frame, in pixels. 0 (the default) is the left edge of the frame. If the placement causes the overlay to extend beyond the right edge of the underlying video, then the overlay is cropped on the right.
--
-- * 'siasasHeight' - The height of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified height. Leave blank to use the native height of the overlay.
--
-- * 'siasasFadeOut' - Applies only if a duration is specified. The time in milliseconds for the image to fade out. The fade-out starts when the duration time is hit, so it effectively extends the duration. Default is 0 (no fade-out).
--
-- * 'siasasWidth' - The width of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified width. Leave blank to use the native width of the overlay.
--
-- * 'siasasOpacity' - Opacity of image where 0 is transparent and 100 is fully opaque. Default is 100.
--
-- * 'siasasLayer' - The number of the layer, 0 to 7. There are 8 layers that can be overlaid on the video, each layer with a different image. The layers are in Z order, which means that overlays with higher values of layer are inserted on top of overlays with lower values of layer. Default is 0.
--
-- * 'siasasDuration' - The duration in milliseconds for the image to remain on the video. If omitted or set to 0 the duration is unlimited and the image will remain until it is explicitly deactivated.
--
-- * 'siasasImageY' - Placement of the top edge of the overlay relative to the top edge of the video frame, in pixels. 0 (the default) is the top edge of the frame. If the placement causes the overlay to extend beyond the bottom edge of the underlying video, then the overlay is cropped on the bottom.
--
-- * 'siasasFadeIn' - The time in milliseconds for the image to fade in. The fade-in starts at the start time of the overlay. Default is 0 (no fade-in).
--
-- * 'siasasImage' - The location and filename of the image file to overlay on the video. The file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in pixels) than the input video.
staticImageActivateScheduleActionSettings
    :: InputLocation -- ^ 'siasasImage'
    -> StaticImageActivateScheduleActionSettings
staticImageActivateScheduleActionSettings pImage_
  = StaticImageActivateScheduleActionSettings'{_siasasImageX
                                                 = Nothing,
                                               _siasasHeight = Nothing,
                                               _siasasFadeOut = Nothing,
                                               _siasasWidth = Nothing,
                                               _siasasOpacity = Nothing,
                                               _siasasLayer = Nothing,
                                               _siasasDuration = Nothing,
                                               _siasasImageY = Nothing,
                                               _siasasFadeIn = Nothing,
                                               _siasasImage = pImage_}

-- | Placement of the left edge of the overlay relative to the left edge of the video frame, in pixels. 0 (the default) is the left edge of the frame. If the placement causes the overlay to extend beyond the right edge of the underlying video, then the overlay is cropped on the right.
siasasImageX :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasImageX = lens _siasasImageX (\ s a -> s{_siasasImageX = a}) . mapping _Nat

-- | The height of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified height. Leave blank to use the native height of the overlay.
siasasHeight :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasHeight = lens _siasasHeight (\ s a -> s{_siasasHeight = a}) . mapping _Nat

-- | Applies only if a duration is specified. The time in milliseconds for the image to fade out. The fade-out starts when the duration time is hit, so it effectively extends the duration. Default is 0 (no fade-out).
siasasFadeOut :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasFadeOut = lens _siasasFadeOut (\ s a -> s{_siasasFadeOut = a}) . mapping _Nat

-- | The width of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified width. Leave blank to use the native width of the overlay.
siasasWidth :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasWidth = lens _siasasWidth (\ s a -> s{_siasasWidth = a}) . mapping _Nat

-- | Opacity of image where 0 is transparent and 100 is fully opaque. Default is 100.
siasasOpacity :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasOpacity = lens _siasasOpacity (\ s a -> s{_siasasOpacity = a}) . mapping _Nat

-- | The number of the layer, 0 to 7. There are 8 layers that can be overlaid on the video, each layer with a different image. The layers are in Z order, which means that overlays with higher values of layer are inserted on top of overlays with lower values of layer. Default is 0.
siasasLayer :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasLayer = lens _siasasLayer (\ s a -> s{_siasasLayer = a}) . mapping _Nat

-- | The duration in milliseconds for the image to remain on the video. If omitted or set to 0 the duration is unlimited and the image will remain until it is explicitly deactivated.
siasasDuration :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasDuration = lens _siasasDuration (\ s a -> s{_siasasDuration = a}) . mapping _Nat

-- | Placement of the top edge of the overlay relative to the top edge of the video frame, in pixels. 0 (the default) is the top edge of the frame. If the placement causes the overlay to extend beyond the bottom edge of the underlying video, then the overlay is cropped on the bottom.
siasasImageY :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasImageY = lens _siasasImageY (\ s a -> s{_siasasImageY = a}) . mapping _Nat

-- | The time in milliseconds for the image to fade in. The fade-in starts at the start time of the overlay. Default is 0 (no fade-in).
siasasFadeIn :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasFadeIn = lens _siasasFadeIn (\ s a -> s{_siasasFadeIn = a}) . mapping _Nat

-- | The location and filename of the image file to overlay on the video. The file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in pixels) than the input video.
siasasImage :: Lens' StaticImageActivateScheduleActionSettings InputLocation
siasasImage = lens _siasasImage (\ s a -> s{_siasasImage = a})

instance FromJSON
           StaticImageActivateScheduleActionSettings
         where
        parseJSON
          = withObject
              "StaticImageActivateScheduleActionSettings"
              (\ x ->
                 StaticImageActivateScheduleActionSettings' <$>
                   (x .:? "imageX") <*> (x .:? "height") <*>
                     (x .:? "fadeOut")
                     <*> (x .:? "width")
                     <*> (x .:? "opacity")
                     <*> (x .:? "layer")
                     <*> (x .:? "duration")
                     <*> (x .:? "imageY")
                     <*> (x .:? "fadeIn")
                     <*> (x .: "image"))

instance Hashable
           StaticImageActivateScheduleActionSettings
         where

instance NFData
           StaticImageActivateScheduleActionSettings
         where

instance ToJSON
           StaticImageActivateScheduleActionSettings
         where
        toJSON StaticImageActivateScheduleActionSettings'{..}
          = object
              (catMaybes
                 [("imageX" .=) <$> _siasasImageX,
                  ("height" .=) <$> _siasasHeight,
                  ("fadeOut" .=) <$> _siasasFadeOut,
                  ("width" .=) <$> _siasasWidth,
                  ("opacity" .=) <$> _siasasOpacity,
                  ("layer" .=) <$> _siasasLayer,
                  ("duration" .=) <$> _siasasDuration,
                  ("imageY" .=) <$> _siasasImageY,
                  ("fadeIn" .=) <$> _siasasFadeIn,
                  Just ("image" .= _siasasImage)])

-- | Settings for the action to deactivate the image in a specific layer.
--
-- /See:/ 'staticImageDeactivateScheduleActionSettings' smart constructor.
data StaticImageDeactivateScheduleActionSettings = StaticImageDeactivateScheduleActionSettings'{_sidsasFadeOut
                                                                                                ::
                                                                                                !(Maybe
                                                                                                    Nat),
                                                                                                _sidsasLayer
                                                                                                ::
                                                                                                !(Maybe
                                                                                                    Nat)}
                                                     deriving (Eq, Read, Show,
                                                               Data, Typeable,
                                                               Generic)

-- | Creates a value of 'StaticImageDeactivateScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sidsasFadeOut' - The time in milliseconds for the image to fade out. Default is 0 (no fade-out).
--
-- * 'sidsasLayer' - The image overlay layer to deactivate, 0 to 7. Default is 0.
staticImageDeactivateScheduleActionSettings
    :: StaticImageDeactivateScheduleActionSettings
staticImageDeactivateScheduleActionSettings
  = StaticImageDeactivateScheduleActionSettings'{_sidsasFadeOut
                                                   = Nothing,
                                                 _sidsasLayer = Nothing}

-- | The time in milliseconds for the image to fade out. Default is 0 (no fade-out).
sidsasFadeOut :: Lens' StaticImageDeactivateScheduleActionSettings (Maybe Natural)
sidsasFadeOut = lens _sidsasFadeOut (\ s a -> s{_sidsasFadeOut = a}) . mapping _Nat

-- | The image overlay layer to deactivate, 0 to 7. Default is 0.
sidsasLayer :: Lens' StaticImageDeactivateScheduleActionSettings (Maybe Natural)
sidsasLayer = lens _sidsasLayer (\ s a -> s{_sidsasLayer = a}) . mapping _Nat

instance FromJSON
           StaticImageDeactivateScheduleActionSettings
         where
        parseJSON
          = withObject
              "StaticImageDeactivateScheduleActionSettings"
              (\ x ->
                 StaticImageDeactivateScheduleActionSettings' <$>
                   (x .:? "fadeOut") <*> (x .:? "layer"))

instance Hashable
           StaticImageDeactivateScheduleActionSettings
         where

instance NFData
           StaticImageDeactivateScheduleActionSettings
         where

instance ToJSON
           StaticImageDeactivateScheduleActionSettings
         where
        toJSON
          StaticImageDeactivateScheduleActionSettings'{..}
          = object
              (catMaybes
                 [("fadeOut" .=) <$> _sidsasFadeOut,
                  ("layer" .=) <$> _sidsasLayer])

-- | Static Key Settings
--
-- /See:/ 'staticKeySettings' smart constructor.
data StaticKeySettings = StaticKeySettings'{_sksKeyProviderServer
                                            :: !(Maybe InputLocation),
                                            _sksStaticKeyValue :: !Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StaticKeySettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sksKeyProviderServer' - The URL of the license server used for protecting content.
--
-- * 'sksStaticKeyValue' - Static key value as a 32 character hexadecimal string.
staticKeySettings
    :: Text -- ^ 'sksStaticKeyValue'
    -> StaticKeySettings
staticKeySettings pStaticKeyValue_
  = StaticKeySettings'{_sksKeyProviderServer = Nothing,
                       _sksStaticKeyValue = pStaticKeyValue_}

-- | The URL of the license server used for protecting content.
sksKeyProviderServer :: Lens' StaticKeySettings (Maybe InputLocation)
sksKeyProviderServer = lens _sksKeyProviderServer (\ s a -> s{_sksKeyProviderServer = a})

-- | Static key value as a 32 character hexadecimal string.
sksStaticKeyValue :: Lens' StaticKeySettings Text
sksStaticKeyValue = lens _sksStaticKeyValue (\ s a -> s{_sksStaticKeyValue = a})

instance FromJSON StaticKeySettings where
        parseJSON
          = withObject "StaticKeySettings"
              (\ x ->
                 StaticKeySettings' <$>
                   (x .:? "keyProviderServer") <*>
                     (x .: "staticKeyValue"))

instance Hashable StaticKeySettings where

instance NFData StaticKeySettings where

instance ToJSON StaticKeySettings where
        toJSON StaticKeySettings'{..}
          = object
              (catMaybes
                 [("keyProviderServer" .=) <$> _sksKeyProviderServer,
                  Just ("staticKeyValue" .= _sksStaticKeyValue)])

-- | Settings to identify the end of the clip.
--
-- /See:/ 'stopTimecode' smart constructor.
data StopTimecode = StopTimecode'{_stLastFrameClippingBehavior
                                  :: !(Maybe LastFrameClippingBehavior),
                                  _stTimecode :: !(Maybe Text)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopTimecode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stLastFrameClippingBehavior' - If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
--
-- * 'stTimecode' - The timecode for the frame where you want to stop the clip. Optional; if not specified, the clip continues to the end of the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
stopTimecode
    :: StopTimecode
stopTimecode
  = StopTimecode'{_stLastFrameClippingBehavior =
                    Nothing,
                  _stTimecode = Nothing}

-- | If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
stLastFrameClippingBehavior :: Lens' StopTimecode (Maybe LastFrameClippingBehavior)
stLastFrameClippingBehavior = lens _stLastFrameClippingBehavior (\ s a -> s{_stLastFrameClippingBehavior = a})

-- | The timecode for the frame where you want to stop the clip. Optional; if not specified, the clip continues to the end of the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
stTimecode :: Lens' StopTimecode (Maybe Text)
stTimecode = lens _stTimecode (\ s a -> s{_stTimecode = a})

instance FromJSON StopTimecode where
        parseJSON
          = withObject "StopTimecode"
              (\ x ->
                 StopTimecode' <$>
                   (x .:? "lastFrameClippingBehavior") <*>
                     (x .:? "timecode"))

instance Hashable StopTimecode where

instance NFData StopTimecode where

instance ToJSON StopTimecode where
        toJSON StopTimecode'{..}
          = object
              (catMaybes
                 [("lastFrameClippingBehavior" .=) <$>
                    _stLastFrameClippingBehavior,
                  ("timecode" .=) <$> _stTimecode])

-- | Teletext Destination Settings
--
-- /See:/ 'teletextDestinationSettings' smart constructor.
data TeletextDestinationSettings = TeletextDestinationSettings'
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'TeletextDestinationSettings' with the minimum fields required to make a request.
--
teletextDestinationSettings
    :: TeletextDestinationSettings
teletextDestinationSettings
  = TeletextDestinationSettings'

instance FromJSON TeletextDestinationSettings where
        parseJSON
          = withObject "TeletextDestinationSettings"
              (\ x -> pure TeletextDestinationSettings')

instance Hashable TeletextDestinationSettings where

instance NFData TeletextDestinationSettings where

instance ToJSON TeletextDestinationSettings where
        toJSON = const (Object mempty)

-- | Teletext Source Settings
--
-- /See:/ 'teletextSourceSettings' smart constructor.
newtype TeletextSourceSettings = TeletextSourceSettings'{_tssPageNumber
                                                         :: Maybe Text}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'TeletextSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tssPageNumber' - Specifies the teletext page number within the data stream from which to extract captions. Range of 0x100 (256) to 0x8FF (2303). Unused for passthrough. Should be specified as a hexadecimal string with no "0x" prefix.
teletextSourceSettings
    :: TeletextSourceSettings
teletextSourceSettings
  = TeletextSourceSettings'{_tssPageNumber = Nothing}

-- | Specifies the teletext page number within the data stream from which to extract captions. Range of 0x100 (256) to 0x8FF (2303). Unused for passthrough. Should be specified as a hexadecimal string with no "0x" prefix.
tssPageNumber :: Lens' TeletextSourceSettings (Maybe Text)
tssPageNumber = lens _tssPageNumber (\ s a -> s{_tssPageNumber = a})

instance FromJSON TeletextSourceSettings where
        parseJSON
          = withObject "TeletextSourceSettings"
              (\ x ->
                 TeletextSourceSettings' <$> (x .:? "pageNumber"))

instance Hashable TeletextSourceSettings where

instance NFData TeletextSourceSettings where

instance ToJSON TeletextSourceSettings where
        toJSON TeletextSourceSettings'{..}
          = object
              (catMaybes [("pageNumber" .=) <$> _tssPageNumber])

-- | Temporal Filter Settings
--
-- /See:/ 'temporalFilterSettings' smart constructor.
data TemporalFilterSettings = TemporalFilterSettings'{_tfsStrength
                                                      ::
                                                      !(Maybe
                                                          TemporalFilterStrength),
                                                      _tfsPostFilterSharpening
                                                      ::
                                                      !(Maybe
                                                          TemporalFilterPostFilterSharpening)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'TemporalFilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfsStrength' - Choose a filter strength. We recommend a strength of 1 or 2. A higher strength might take out good information, resulting in an image that is overly soft.
--
-- * 'tfsPostFilterSharpening' - If you enable this filter, the results are the following: - If the source content is noisy (it contains excessive digital artifacts), the filter cleans up the source. - If the source content is already clean, the filter tends to decrease the bitrate, especially when the rate control mode is QVBR.
temporalFilterSettings
    :: TemporalFilterSettings
temporalFilterSettings
  = TemporalFilterSettings'{_tfsStrength = Nothing,
                            _tfsPostFilterSharpening = Nothing}

-- | Choose a filter strength. We recommend a strength of 1 or 2. A higher strength might take out good information, resulting in an image that is overly soft.
tfsStrength :: Lens' TemporalFilterSettings (Maybe TemporalFilterStrength)
tfsStrength = lens _tfsStrength (\ s a -> s{_tfsStrength = a})

-- | If you enable this filter, the results are the following: - If the source content is noisy (it contains excessive digital artifacts), the filter cleans up the source. - If the source content is already clean, the filter tends to decrease the bitrate, especially when the rate control mode is QVBR.
tfsPostFilterSharpening :: Lens' TemporalFilterSettings (Maybe TemporalFilterPostFilterSharpening)
tfsPostFilterSharpening = lens _tfsPostFilterSharpening (\ s a -> s{_tfsPostFilterSharpening = a})

instance FromJSON TemporalFilterSettings where
        parseJSON
          = withObject "TemporalFilterSettings"
              (\ x ->
                 TemporalFilterSettings' <$>
                   (x .:? "strength") <*>
                     (x .:? "postFilterSharpening"))

instance Hashable TemporalFilterSettings where

instance NFData TemporalFilterSettings where

instance ToJSON TemporalFilterSettings where
        toJSON TemporalFilterSettings'{..}
          = object
              (catMaybes
                 [("strength" .=) <$> _tfsStrength,
                  ("postFilterSharpening" .=) <$>
                    _tfsPostFilterSharpening])

-- | Timecode Config
--
-- /See:/ 'timecodeConfig' smart constructor.
data TimecodeConfig = TimecodeConfig'{_tcSyncThreshold
                                      :: !(Maybe Nat),
                                      _tcSource :: !TimecodeConfigSource}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimecodeConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcSyncThreshold' - Threshold in frames beyond which output timecode is resynchronized to the input timecode. Discrepancies below this threshold are permitted to avoid unnecessary discontinuities in the output timecode. No timecode sync when this is not specified.
--
-- * 'tcSource' - Identifies the source for the timecode that will be associated with the events outputs. -Embedded (embedded): Initialize the output timecode with timecode from the the source.  If no embedded timecode is detected in the source, the system falls back to using "Start at 0" (zerobased). -System Clock (systemclock): Use the UTC time. -Start at 0 (zerobased): The time of the first frame of the event will be 00:00:00:00.
timecodeConfig
    :: TimecodeConfigSource -- ^ 'tcSource'
    -> TimecodeConfig
timecodeConfig pSource_
  = TimecodeConfig'{_tcSyncThreshold = Nothing,
                    _tcSource = pSource_}

-- | Threshold in frames beyond which output timecode is resynchronized to the input timecode. Discrepancies below this threshold are permitted to avoid unnecessary discontinuities in the output timecode. No timecode sync when this is not specified.
tcSyncThreshold :: Lens' TimecodeConfig (Maybe Natural)
tcSyncThreshold = lens _tcSyncThreshold (\ s a -> s{_tcSyncThreshold = a}) . mapping _Nat

-- | Identifies the source for the timecode that will be associated with the events outputs. -Embedded (embedded): Initialize the output timecode with timecode from the the source.  If no embedded timecode is detected in the source, the system falls back to using "Start at 0" (zerobased). -System Clock (systemclock): Use the UTC time. -Start at 0 (zerobased): The time of the first frame of the event will be 00:00:00:00.
tcSource :: Lens' TimecodeConfig TimecodeConfigSource
tcSource = lens _tcSource (\ s a -> s{_tcSource = a})

instance FromJSON TimecodeConfig where
        parseJSON
          = withObject "TimecodeConfig"
              (\ x ->
                 TimecodeConfig' <$>
                   (x .:? "syncThreshold") <*> (x .: "source"))

instance Hashable TimecodeConfig where

instance NFData TimecodeConfig where

instance ToJSON TimecodeConfig where
        toJSON TimecodeConfig'{..}
          = object
              (catMaybes
                 [("syncThreshold" .=) <$> _tcSyncThreshold,
                  Just ("source" .= _tcSource)])

-- | Ttml Destination Settings
--
-- /See:/ 'ttmlDestinationSettings' smart constructor.
newtype TtmlDestinationSettings = TtmlDestinationSettings'{_tdsStyleControl
                                                           ::
                                                           Maybe
                                                             TtmlDestinationStyleControl}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'TtmlDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdsStyleControl' - When set to passthrough, passes through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
ttmlDestinationSettings
    :: TtmlDestinationSettings
ttmlDestinationSettings
  = TtmlDestinationSettings'{_tdsStyleControl =
                               Nothing}

-- | When set to passthrough, passes through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
tdsStyleControl :: Lens' TtmlDestinationSettings (Maybe TtmlDestinationStyleControl)
tdsStyleControl = lens _tdsStyleControl (\ s a -> s{_tdsStyleControl = a})

instance FromJSON TtmlDestinationSettings where
        parseJSON
          = withObject "TtmlDestinationSettings"
              (\ x ->
                 TtmlDestinationSettings' <$> (x .:? "styleControl"))

instance Hashable TtmlDestinationSettings where

instance NFData TtmlDestinationSettings where

instance ToJSON TtmlDestinationSettings where
        toJSON TtmlDestinationSettings'{..}
          = object
              (catMaybes
                 [("styleControl" .=) <$> _tdsStyleControl])

-- | Udp Container Settings
--
-- /See:/ 'udpContainerSettings' smart constructor.
newtype UdpContainerSettings = UdpContainerSettings'{_ucsM2tsSettings
                                                     :: Maybe M2tsSettings}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'UdpContainerSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsM2tsSettings' - Undocumented member.
udpContainerSettings
    :: UdpContainerSettings
udpContainerSettings
  = UdpContainerSettings'{_ucsM2tsSettings = Nothing}

-- | Undocumented member.
ucsM2tsSettings :: Lens' UdpContainerSettings (Maybe M2tsSettings)
ucsM2tsSettings = lens _ucsM2tsSettings (\ s a -> s{_ucsM2tsSettings = a})

instance FromJSON UdpContainerSettings where
        parseJSON
          = withObject "UdpContainerSettings"
              (\ x ->
                 UdpContainerSettings' <$> (x .:? "m2tsSettings"))

instance Hashable UdpContainerSettings where

instance NFData UdpContainerSettings where

instance ToJSON UdpContainerSettings where
        toJSON UdpContainerSettings'{..}
          = object
              (catMaybes
                 [("m2tsSettings" .=) <$> _ucsM2tsSettings])

-- | Udp Group Settings
--
-- /See:/ 'udpGroupSettings' smart constructor.
data UdpGroupSettings = UdpGroupSettings'{_ugsTimedMetadataId3Period
                                          :: !(Maybe Nat),
                                          _ugsInputLossAction ::
                                          !(Maybe InputLossActionForUdpOut),
                                          _ugsTimedMetadataId3Frame ::
                                          !(Maybe UdpTimedMetadataId3Frame)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UdpGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsTimedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- * 'ugsInputLossAction' - Specifies behavior of last resort when input video is lost, and no more backup inputs are available. When dropTs is selected the entire transport stream will stop being emitted.  When dropProgram is selected the program can be dropped from the transport stream (and replaced with null packets to meet the TS bitrate requirement).  Or, when emitProgram is chosen the transport stream will continue to be produced normally with repeat frames, black frames, or slate frames substituted for the absent input video.
--
-- * 'ugsTimedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
udpGroupSettings
    :: UdpGroupSettings
udpGroupSettings
  = UdpGroupSettings'{_ugsTimedMetadataId3Period =
                        Nothing,
                      _ugsInputLossAction = Nothing,
                      _ugsTimedMetadataId3Frame = Nothing}

-- | Timed Metadata interval in seconds.
ugsTimedMetadataId3Period :: Lens' UdpGroupSettings (Maybe Natural)
ugsTimedMetadataId3Period = lens _ugsTimedMetadataId3Period (\ s a -> s{_ugsTimedMetadataId3Period = a}) . mapping _Nat

-- | Specifies behavior of last resort when input video is lost, and no more backup inputs are available. When dropTs is selected the entire transport stream will stop being emitted.  When dropProgram is selected the program can be dropped from the transport stream (and replaced with null packets to meet the TS bitrate requirement).  Or, when emitProgram is chosen the transport stream will continue to be produced normally with repeat frames, black frames, or slate frames substituted for the absent input video.
ugsInputLossAction :: Lens' UdpGroupSettings (Maybe InputLossActionForUdpOut)
ugsInputLossAction = lens _ugsInputLossAction (\ s a -> s{_ugsInputLossAction = a})

-- | Indicates ID3 frame that has the timecode.
ugsTimedMetadataId3Frame :: Lens' UdpGroupSettings (Maybe UdpTimedMetadataId3Frame)
ugsTimedMetadataId3Frame = lens _ugsTimedMetadataId3Frame (\ s a -> s{_ugsTimedMetadataId3Frame = a})

instance FromJSON UdpGroupSettings where
        parseJSON
          = withObject "UdpGroupSettings"
              (\ x ->
                 UdpGroupSettings' <$>
                   (x .:? "timedMetadataId3Period") <*>
                     (x .:? "inputLossAction")
                     <*> (x .:? "timedMetadataId3Frame"))

instance Hashable UdpGroupSettings where

instance NFData UdpGroupSettings where

instance ToJSON UdpGroupSettings where
        toJSON UdpGroupSettings'{..}
          = object
              (catMaybes
                 [("timedMetadataId3Period" .=) <$>
                    _ugsTimedMetadataId3Period,
                  ("inputLossAction" .=) <$> _ugsInputLossAction,
                  ("timedMetadataId3Frame" .=) <$>
                    _ugsTimedMetadataId3Frame])

-- | Udp Output Settings
--
-- /See:/ 'udpOutputSettings' smart constructor.
data UdpOutputSettings = UdpOutputSettings'{_uosFecOutputSettings
                                            :: !(Maybe FecOutputSettings),
                                            _uosBufferMsec :: !(Maybe Nat),
                                            _uosDestination ::
                                            !OutputLocationRef,
                                            _uosContainerSettings ::
                                            !UdpContainerSettings}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UdpOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uosFecOutputSettings' - Settings for enabling and adjusting Forward Error Correction on UDP outputs.
--
-- * 'uosBufferMsec' - UDP output buffering in milliseconds. Larger values increase latency through the transcoder but simultaneously assist the transcoder in maintaining a constant, low-jitter UDP/RTP output while accommodating clock recovery, input switching, input disruptions, picture reordering, etc.
--
-- * 'uosDestination' - Destination address and port number for RTP or UDP packets. Can be unicast or multicast RTP or UDP (eg. rtp://239.10.10.10:5001 or udp://10.100.100.100:5002).
--
-- * 'uosContainerSettings' - Undocumented member.
udpOutputSettings
    :: OutputLocationRef -- ^ 'uosDestination'
    -> UdpContainerSettings -- ^ 'uosContainerSettings'
    -> UdpOutputSettings
udpOutputSettings pDestination_ pContainerSettings_
  = UdpOutputSettings'{_uosFecOutputSettings = Nothing,
                       _uosBufferMsec = Nothing,
                       _uosDestination = pDestination_,
                       _uosContainerSettings = pContainerSettings_}

-- | Settings for enabling and adjusting Forward Error Correction on UDP outputs.
uosFecOutputSettings :: Lens' UdpOutputSettings (Maybe FecOutputSettings)
uosFecOutputSettings = lens _uosFecOutputSettings (\ s a -> s{_uosFecOutputSettings = a})

-- | UDP output buffering in milliseconds. Larger values increase latency through the transcoder but simultaneously assist the transcoder in maintaining a constant, low-jitter UDP/RTP output while accommodating clock recovery, input switching, input disruptions, picture reordering, etc.
uosBufferMsec :: Lens' UdpOutputSettings (Maybe Natural)
uosBufferMsec = lens _uosBufferMsec (\ s a -> s{_uosBufferMsec = a}) . mapping _Nat

-- | Destination address and port number for RTP or UDP packets. Can be unicast or multicast RTP or UDP (eg. rtp://239.10.10.10:5001 or udp://10.100.100.100:5002).
uosDestination :: Lens' UdpOutputSettings OutputLocationRef
uosDestination = lens _uosDestination (\ s a -> s{_uosDestination = a})

-- | Undocumented member.
uosContainerSettings :: Lens' UdpOutputSettings UdpContainerSettings
uosContainerSettings = lens _uosContainerSettings (\ s a -> s{_uosContainerSettings = a})

instance FromJSON UdpOutputSettings where
        parseJSON
          = withObject "UdpOutputSettings"
              (\ x ->
                 UdpOutputSettings' <$>
                   (x .:? "fecOutputSettings") <*> (x .:? "bufferMsec")
                     <*> (x .: "destination")
                     <*> (x .: "containerSettings"))

instance Hashable UdpOutputSettings where

instance NFData UdpOutputSettings where

instance ToJSON UdpOutputSettings where
        toJSON UdpOutputSettings'{..}
          = object
              (catMaybes
                 [("fecOutputSettings" .=) <$> _uosFecOutputSettings,
                  ("bufferMsec" .=) <$> _uosBufferMsec,
                  Just ("destination" .= _uosDestination),
                  Just ("containerSettings" .= _uosContainerSettings)])

-- | Video Codec Settings
--
-- /See:/ 'videoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'{_vcsFrameCaptureSettings
                                              :: !(Maybe FrameCaptureSettings),
                                              _vcsH265Settings ::
                                              !(Maybe H265Settings),
                                              _vcsH264Settings ::
                                              !(Maybe H264Settings)}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoCodecSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcsFrameCaptureSettings' - Undocumented member.
--
-- * 'vcsH265Settings' - Undocumented member.
--
-- * 'vcsH264Settings' - Undocumented member.
videoCodecSettings
    :: VideoCodecSettings
videoCodecSettings
  = VideoCodecSettings'{_vcsFrameCaptureSettings =
                          Nothing,
                        _vcsH265Settings = Nothing,
                        _vcsH264Settings = Nothing}

-- | Undocumented member.
vcsFrameCaptureSettings :: Lens' VideoCodecSettings (Maybe FrameCaptureSettings)
vcsFrameCaptureSettings = lens _vcsFrameCaptureSettings (\ s a -> s{_vcsFrameCaptureSettings = a})

-- | Undocumented member.
vcsH265Settings :: Lens' VideoCodecSettings (Maybe H265Settings)
vcsH265Settings = lens _vcsH265Settings (\ s a -> s{_vcsH265Settings = a})

-- | Undocumented member.
vcsH264Settings :: Lens' VideoCodecSettings (Maybe H264Settings)
vcsH264Settings = lens _vcsH264Settings (\ s a -> s{_vcsH264Settings = a})

instance FromJSON VideoCodecSettings where
        parseJSON
          = withObject "VideoCodecSettings"
              (\ x ->
                 VideoCodecSettings' <$>
                   (x .:? "frameCaptureSettings") <*>
                     (x .:? "h265Settings")
                     <*> (x .:? "h264Settings"))

instance Hashable VideoCodecSettings where

instance NFData VideoCodecSettings where

instance ToJSON VideoCodecSettings where
        toJSON VideoCodecSettings'{..}
          = object
              (catMaybes
                 [("frameCaptureSettings" .=) <$>
                    _vcsFrameCaptureSettings,
                  ("h265Settings" .=) <$> _vcsH265Settings,
                  ("h264Settings" .=) <$> _vcsH264Settings])

-- | Video settings for this stream.
--
-- /See:/ 'videoDescription' smart constructor.
data VideoDescription = VideoDescription'{_vdHeight
                                          :: !(Maybe Int),
                                          _vdSharpness :: !(Maybe Nat),
                                          _vdWidth :: !(Maybe Int),
                                          _vdScalingBehavior ::
                                          !(Maybe
                                              VideoDescriptionScalingBehavior),
                                          _vdRespondToAfd ::
                                          !(Maybe VideoDescriptionRespondToAfd),
                                          _vdCodecSettings ::
                                          !(Maybe VideoCodecSettings),
                                          _vdName :: !Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdHeight' - Output video height, in pixels. Must be an even number. For most codecs, you can leave this field and width blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
--
-- * 'vdSharpness' - Changes the strength of the anti-alias filter used for scaling. 0 is the softest setting, 100 is the sharpest. A setting of 50 is recommended for most content.
--
-- * 'vdWidth' - Output video width, in pixels. Must be an even number. For most codecs, you can leave this field and height blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
--
-- * 'vdScalingBehavior' - STRETCH_TO_OUTPUT configures the output position to stretch the video to the specified output resolution (height and width). This option will override any position value. DEFAULT may insert black boxes (pillar boxes or letter boxes) around the video to provide the specified output resolution.
--
-- * 'vdRespondToAfd' - Indicates how to respond to the AFD values in the input stream. RESPOND causes input video to be clipped, depending on the AFD value, input display aspect ratio, and output display aspect ratio, and (except for FRAME_CAPTURE codec) includes the values in the output. PASSTHROUGH (does not apply to FRAME_CAPTURE codec) ignores the AFD values and includes the values in the output, so input video is not clipped. NONE ignores the AFD values and does not include the values through to the output, so input video is not clipped.
--
-- * 'vdCodecSettings' - Video codec settings.
--
-- * 'vdName' - The name of this VideoDescription. Outputs will use this name to uniquely identify this Description.  Description names should be unique within this Live Event.
videoDescription
    :: Text -- ^ 'vdName'
    -> VideoDescription
videoDescription pName_
  = VideoDescription'{_vdHeight = Nothing,
                      _vdSharpness = Nothing, _vdWidth = Nothing,
                      _vdScalingBehavior = Nothing,
                      _vdRespondToAfd = Nothing,
                      _vdCodecSettings = Nothing, _vdName = pName_}

-- | Output video height, in pixels. Must be an even number. For most codecs, you can leave this field and width blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
vdHeight :: Lens' VideoDescription (Maybe Int)
vdHeight = lens _vdHeight (\ s a -> s{_vdHeight = a})

-- | Changes the strength of the anti-alias filter used for scaling. 0 is the softest setting, 100 is the sharpest. A setting of 50 is recommended for most content.
vdSharpness :: Lens' VideoDescription (Maybe Natural)
vdSharpness = lens _vdSharpness (\ s a -> s{_vdSharpness = a}) . mapping _Nat

-- | Output video width, in pixels. Must be an even number. For most codecs, you can leave this field and height blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
vdWidth :: Lens' VideoDescription (Maybe Int)
vdWidth = lens _vdWidth (\ s a -> s{_vdWidth = a})

-- | STRETCH_TO_OUTPUT configures the output position to stretch the video to the specified output resolution (height and width). This option will override any position value. DEFAULT may insert black boxes (pillar boxes or letter boxes) around the video to provide the specified output resolution.
vdScalingBehavior :: Lens' VideoDescription (Maybe VideoDescriptionScalingBehavior)
vdScalingBehavior = lens _vdScalingBehavior (\ s a -> s{_vdScalingBehavior = a})

-- | Indicates how to respond to the AFD values in the input stream. RESPOND causes input video to be clipped, depending on the AFD value, input display aspect ratio, and output display aspect ratio, and (except for FRAME_CAPTURE codec) includes the values in the output. PASSTHROUGH (does not apply to FRAME_CAPTURE codec) ignores the AFD values and includes the values in the output, so input video is not clipped. NONE ignores the AFD values and does not include the values through to the output, so input video is not clipped.
vdRespondToAfd :: Lens' VideoDescription (Maybe VideoDescriptionRespondToAfd)
vdRespondToAfd = lens _vdRespondToAfd (\ s a -> s{_vdRespondToAfd = a})

-- | Video codec settings.
vdCodecSettings :: Lens' VideoDescription (Maybe VideoCodecSettings)
vdCodecSettings = lens _vdCodecSettings (\ s a -> s{_vdCodecSettings = a})

-- | The name of this VideoDescription. Outputs will use this name to uniquely identify this Description.  Description names should be unique within this Live Event.
vdName :: Lens' VideoDescription Text
vdName = lens _vdName (\ s a -> s{_vdName = a})

instance FromJSON VideoDescription where
        parseJSON
          = withObject "VideoDescription"
              (\ x ->
                 VideoDescription' <$>
                   (x .:? "height") <*> (x .:? "sharpness") <*>
                     (x .:? "width")
                     <*> (x .:? "scalingBehavior")
                     <*> (x .:? "respondToAfd")
                     <*> (x .:? "codecSettings")
                     <*> (x .: "name"))

instance Hashable VideoDescription where

instance NFData VideoDescription where

instance ToJSON VideoDescription where
        toJSON VideoDescription'{..}
          = object
              (catMaybes
                 [("height" .=) <$> _vdHeight,
                  ("sharpness" .=) <$> _vdSharpness,
                  ("width" .=) <$> _vdWidth,
                  ("scalingBehavior" .=) <$> _vdScalingBehavior,
                  ("respondToAfd" .=) <$> _vdRespondToAfd,
                  ("codecSettings" .=) <$> _vdCodecSettings,
                  Just ("name" .= _vdName)])

-- | Specifies a particular video stream within an input source. An input may have only a single video selector.
--
-- /See:/ 'videoSelector' smart constructor.
data VideoSelector = VideoSelector'{_vsSelectorSettings
                                    :: !(Maybe VideoSelectorSettings),
                                    _vsColorSpaceUsage ::
                                    !(Maybe VideoSelectorColorSpaceUsage),
                                    _vsColorSpace ::
                                    !(Maybe VideoSelectorColorSpace)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsSelectorSettings' - The video selector settings.
--
-- * 'vsColorSpaceUsage' - Applies only if colorSpace is a value other than follow. This field controls how the value in the colorSpace field will be used. fallback means that when the input does include color space data, that data will be used, but when the input has no color space data, the value in colorSpace will be used. Choose fallback if your input is sometimes missing color space data, but when it does have color space data, that data is correct. force means to always use the value in colorSpace. Choose force if your input usually has no color space data or might have unreliable color space data.
--
-- * 'vsColorSpace' - Specifies the color space of an input. This setting works in tandem with colorSpaceUsage and a video description's colorSpaceSettingsChoice to determine if any conversion will be performed.
videoSelector
    :: VideoSelector
videoSelector
  = VideoSelector'{_vsSelectorSettings = Nothing,
                   _vsColorSpaceUsage = Nothing,
                   _vsColorSpace = Nothing}

-- | The video selector settings.
vsSelectorSettings :: Lens' VideoSelector (Maybe VideoSelectorSettings)
vsSelectorSettings = lens _vsSelectorSettings (\ s a -> s{_vsSelectorSettings = a})

-- | Applies only if colorSpace is a value other than follow. This field controls how the value in the colorSpace field will be used. fallback means that when the input does include color space data, that data will be used, but when the input has no color space data, the value in colorSpace will be used. Choose fallback if your input is sometimes missing color space data, but when it does have color space data, that data is correct. force means to always use the value in colorSpace. Choose force if your input usually has no color space data or might have unreliable color space data.
vsColorSpaceUsage :: Lens' VideoSelector (Maybe VideoSelectorColorSpaceUsage)
vsColorSpaceUsage = lens _vsColorSpaceUsage (\ s a -> s{_vsColorSpaceUsage = a})

-- | Specifies the color space of an input. This setting works in tandem with colorSpaceUsage and a video description's colorSpaceSettingsChoice to determine if any conversion will be performed.
vsColorSpace :: Lens' VideoSelector (Maybe VideoSelectorColorSpace)
vsColorSpace = lens _vsColorSpace (\ s a -> s{_vsColorSpace = a})

instance FromJSON VideoSelector where
        parseJSON
          = withObject "VideoSelector"
              (\ x ->
                 VideoSelector' <$>
                   (x .:? "selectorSettings") <*>
                     (x .:? "colorSpaceUsage")
                     <*> (x .:? "colorSpace"))

instance Hashable VideoSelector where

instance NFData VideoSelector where

instance ToJSON VideoSelector where
        toJSON VideoSelector'{..}
          = object
              (catMaybes
                 [("selectorSettings" .=) <$> _vsSelectorSettings,
                  ("colorSpaceUsage" .=) <$> _vsColorSpaceUsage,
                  ("colorSpace" .=) <$> _vsColorSpace])

-- | Video Selector Pid
--
-- /See:/ 'videoSelectorPid' smart constructor.
newtype VideoSelectorPid = VideoSelectorPid'{_vspPid
                                             :: Maybe Nat}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoSelectorPid' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vspPid' - Selects a specific PID from within a video source.
videoSelectorPid
    :: VideoSelectorPid
videoSelectorPid
  = VideoSelectorPid'{_vspPid = Nothing}

-- | Selects a specific PID from within a video source.
vspPid :: Lens' VideoSelectorPid (Maybe Natural)
vspPid = lens _vspPid (\ s a -> s{_vspPid = a}) . mapping _Nat

instance FromJSON VideoSelectorPid where
        parseJSON
          = withObject "VideoSelectorPid"
              (\ x -> VideoSelectorPid' <$> (x .:? "pid"))

instance Hashable VideoSelectorPid where

instance NFData VideoSelectorPid where

instance ToJSON VideoSelectorPid where
        toJSON VideoSelectorPid'{..}
          = object (catMaybes [("pid" .=) <$> _vspPid])

-- | Video Selector Program Id
--
-- /See:/ 'videoSelectorProgramId' smart constructor.
newtype VideoSelectorProgramId = VideoSelectorProgramId'{_vspiProgramId
                                                         :: Maybe Nat}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'VideoSelectorProgramId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vspiProgramId' - Selects a specific program from within a multi-program transport stream. If the program doesn't exist, the first program within the transport stream will be selected by default.
videoSelectorProgramId
    :: VideoSelectorProgramId
videoSelectorProgramId
  = VideoSelectorProgramId'{_vspiProgramId = Nothing}

-- | Selects a specific program from within a multi-program transport stream. If the program doesn't exist, the first program within the transport stream will be selected by default.
vspiProgramId :: Lens' VideoSelectorProgramId (Maybe Natural)
vspiProgramId = lens _vspiProgramId (\ s a -> s{_vspiProgramId = a}) . mapping _Nat

instance FromJSON VideoSelectorProgramId where
        parseJSON
          = withObject "VideoSelectorProgramId"
              (\ x ->
                 VideoSelectorProgramId' <$> (x .:? "programId"))

instance Hashable VideoSelectorProgramId where

instance NFData VideoSelectorProgramId where

instance ToJSON VideoSelectorProgramId where
        toJSON VideoSelectorProgramId'{..}
          = object
              (catMaybes [("programId" .=) <$> _vspiProgramId])

-- | Video Selector Settings
--
-- /See:/ 'videoSelectorSettings' smart constructor.
data VideoSelectorSettings = VideoSelectorSettings'{_vssVideoSelectorProgramId
                                                    ::
                                                    !(Maybe
                                                        VideoSelectorProgramId),
                                                    _vssVideoSelectorPid ::
                                                    !(Maybe VideoSelectorPid)}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'VideoSelectorSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vssVideoSelectorProgramId' - Undocumented member.
--
-- * 'vssVideoSelectorPid' - Undocumented member.
videoSelectorSettings
    :: VideoSelectorSettings
videoSelectorSettings
  = VideoSelectorSettings'{_vssVideoSelectorProgramId =
                             Nothing,
                           _vssVideoSelectorPid = Nothing}

-- | Undocumented member.
vssVideoSelectorProgramId :: Lens' VideoSelectorSettings (Maybe VideoSelectorProgramId)
vssVideoSelectorProgramId = lens _vssVideoSelectorProgramId (\ s a -> s{_vssVideoSelectorProgramId = a})

-- | Undocumented member.
vssVideoSelectorPid :: Lens' VideoSelectorSettings (Maybe VideoSelectorPid)
vssVideoSelectorPid = lens _vssVideoSelectorPid (\ s a -> s{_vssVideoSelectorPid = a})

instance FromJSON VideoSelectorSettings where
        parseJSON
          = withObject "VideoSelectorSettings"
              (\ x ->
                 VideoSelectorSettings' <$>
                   (x .:? "videoSelectorProgramId") <*>
                     (x .:? "videoSelectorPid"))

instance Hashable VideoSelectorSettings where

instance NFData VideoSelectorSettings where

instance ToJSON VideoSelectorSettings where
        toJSON VideoSelectorSettings'{..}
          = object
              (catMaybes
                 [("videoSelectorProgramId" .=) <$>
                    _vssVideoSelectorProgramId,
                  ("videoSelectorPid" .=) <$> _vssVideoSelectorPid])

-- | Webvtt Destination Settings
--
-- /See:/ 'webvttDestinationSettings' smart constructor.
data WebvttDestinationSettings = WebvttDestinationSettings'
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'WebvttDestinationSettings' with the minimum fields required to make a request.
--
webvttDestinationSettings
    :: WebvttDestinationSettings
webvttDestinationSettings
  = WebvttDestinationSettings'

instance FromJSON WebvttDestinationSettings where
        parseJSON
          = withObject "WebvttDestinationSettings"
              (\ x -> pure WebvttDestinationSettings')

instance Hashable WebvttDestinationSettings where

instance NFData WebvttDestinationSettings where

instance ToJSON WebvttDestinationSettings where
        toJSON = const (Object mempty)
