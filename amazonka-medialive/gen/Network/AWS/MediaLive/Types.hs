{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types
    (
    -- * Service Configuration
      mediaLive

    -- * Errors
    , _GatewayTimeoutException
    , _UnprocessableEntityException
    , _ConflictException
    , _ForbiddenException
    , _NotFoundException
    , _TooManyRequestsException
    , _InternalServerErrorException
    , _BadGatewayException
    , _BadRequestException

    -- * AacCodingMode
    , AacCodingMode (..)

    -- * AacInputType
    , AacInputType (..)

    -- * AacProfile
    , AacProfile (..)

    -- * AacRateControlMode
    , AacRateControlMode (..)

    -- * AacRawFormat
    , AacRawFormat (..)

    -- * AacSpec
    , AacSpec (..)

    -- * AacVbrQuality
    , AacVbrQuality (..)

    -- * Ac3BitstreamMode
    , Ac3BitstreamMode (..)

    -- * Ac3CodingMode
    , Ac3CodingMode (..)

    -- * Ac3DrcProfile
    , Ac3DrcProfile (..)

    -- * Ac3LfeFilter
    , Ac3LfeFilter (..)

    -- * Ac3MetadataControl
    , Ac3MetadataControl (..)

    -- * AfdSignaling
    , AfdSignaling (..)

    -- * AudioDescriptionAudioTypeControl
    , AudioDescriptionAudioTypeControl (..)

    -- * AudioDescriptionLanguageCodeControl
    , AudioDescriptionLanguageCodeControl (..)

    -- * AudioLanguageSelectionPolicy
    , AudioLanguageSelectionPolicy (..)

    -- * AudioNormalizationAlgorithm
    , AudioNormalizationAlgorithm (..)

    -- * AudioNormalizationAlgorithmControl
    , AudioNormalizationAlgorithmControl (..)

    -- * AudioOnlyHlsSegmentType
    , AudioOnlyHlsSegmentType (..)

    -- * AudioOnlyHlsTrackType
    , AudioOnlyHlsTrackType (..)

    -- * AudioType
    , AudioType (..)

    -- * AuthenticationScheme
    , AuthenticationScheme (..)

    -- * AvailBlankingState
    , AvailBlankingState (..)

    -- * BlackoutSlateNetworkEndBlackout
    , BlackoutSlateNetworkEndBlackout (..)

    -- * BlackoutSlateState
    , BlackoutSlateState (..)

    -- * BurnInAlignment
    , BurnInAlignment (..)

    -- * BurnInBackgroundColor
    , BurnInBackgroundColor (..)

    -- * BurnInFontColor
    , BurnInFontColor (..)

    -- * BurnInOutlineColor
    , BurnInOutlineColor (..)

    -- * BurnInShadowColor
    , BurnInShadowColor (..)

    -- * BurnInTeletextGridControl
    , BurnInTeletextGridControl (..)

    -- * ChannelClass
    , ChannelClass (..)

    -- * ChannelState
    , ChannelState (..)

    -- * DeviceSettingsSyncState
    , DeviceSettingsSyncState (..)

    -- * DvbSdtOutputSdt
    , DvbSdtOutputSdt (..)

    -- * DvbSubDestinationAlignment
    , DvbSubDestinationAlignment (..)

    -- * DvbSubDestinationBackgroundColor
    , DvbSubDestinationBackgroundColor (..)

    -- * DvbSubDestinationFontColor
    , DvbSubDestinationFontColor (..)

    -- * DvbSubDestinationOutlineColor
    , DvbSubDestinationOutlineColor (..)

    -- * DvbSubDestinationShadowColor
    , DvbSubDestinationShadowColor (..)

    -- * DvbSubDestinationTeletextGridControl
    , DvbSubDestinationTeletextGridControl (..)

    -- * Eac3AttenuationControl
    , Eac3AttenuationControl (..)

    -- * Eac3BitstreamMode
    , Eac3BitstreamMode (..)

    -- * Eac3CodingMode
    , Eac3CodingMode (..)

    -- * Eac3DcFilter
    , Eac3DcFilter (..)

    -- * Eac3DrcLine
    , Eac3DrcLine (..)

    -- * Eac3DrcRf
    , Eac3DrcRf (..)

    -- * Eac3LfeControl
    , Eac3LfeControl (..)

    -- * Eac3LfeFilter
    , Eac3LfeFilter (..)

    -- * Eac3MetadataControl
    , Eac3MetadataControl (..)

    -- * Eac3PassthroughControl
    , Eac3PassthroughControl (..)

    -- * Eac3PhaseControl
    , Eac3PhaseControl (..)

    -- * Eac3StereoDownmix
    , Eac3StereoDownmix (..)

    -- * Eac3SurroundExMode
    , Eac3SurroundExMode (..)

    -- * Eac3SurroundMode
    , Eac3SurroundMode (..)

    -- * EmbeddedConvert608To708
    , EmbeddedConvert608To708 (..)

    -- * EmbeddedScte20Detection
    , EmbeddedScte20Detection (..)

    -- * FeatureActivationsInputPrepareScheduleActions
    , FeatureActivationsInputPrepareScheduleActions (..)

    -- * FecOutputIncludeFec
    , FecOutputIncludeFec (..)

    -- * FixedAfd
    , FixedAfd (..)

    -- * Fmp4NielsenId3Behavior
    , Fmp4NielsenId3Behavior (..)

    -- * Fmp4TimedMetadataBehavior
    , Fmp4TimedMetadataBehavior (..)

    -- * FollowPoint
    , FollowPoint (..)

    -- * FrameCaptureIntervalUnit
    , FrameCaptureIntervalUnit (..)

    -- * GlobalConfigurationInputEndAction
    , GlobalConfigurationInputEndAction (..)

    -- * GlobalConfigurationLowFramerateInputs
    , GlobalConfigurationLowFramerateInputs (..)

    -- * GlobalConfigurationOutputLockingMode
    , GlobalConfigurationOutputLockingMode (..)

    -- * GlobalConfigurationOutputTimingSource
    , GlobalConfigurationOutputTimingSource (..)

    -- * H264AdaptiveQuantization
    , H264AdaptiveQuantization (..)

    -- * H264ColorMetadata
    , H264ColorMetadata (..)

    -- * H264EntropyEncoding
    , H264EntropyEncoding (..)

    -- * H264FlickerAq
    , H264FlickerAq (..)

    -- * H264ForceFieldPictures
    , H264ForceFieldPictures (..)

    -- * H264FramerateControl
    , H264FramerateControl (..)

    -- * H264GopBReference
    , H264GopBReference (..)

    -- * H264GopSizeUnits
    , H264GopSizeUnits (..)

    -- * H264Level
    , H264Level (..)

    -- * H264LookAheadRateControl
    , H264LookAheadRateControl (..)

    -- * H264ParControl
    , H264ParControl (..)

    -- * H264Profile
    , H264Profile (..)

    -- * H264QualityLevel
    , H264QualityLevel (..)

    -- * H264RateControlMode
    , H264RateControlMode (..)

    -- * H264ScanType
    , H264ScanType (..)

    -- * H264SceneChangeDetect
    , H264SceneChangeDetect (..)

    -- * H264SpatialAq
    , H264SpatialAq (..)

    -- * H264SubGopLength
    , H264SubGopLength (..)

    -- * H264Syntax
    , H264Syntax (..)

    -- * H264TemporalAq
    , H264TemporalAq (..)

    -- * H264TimecodeInsertionBehavior
    , H264TimecodeInsertionBehavior (..)

    -- * H265AdaptiveQuantization
    , H265AdaptiveQuantization (..)

    -- * H265AlternativeTransferFunction
    , H265AlternativeTransferFunction (..)

    -- * H265ColorMetadata
    , H265ColorMetadata (..)

    -- * H265FlickerAq
    , H265FlickerAq (..)

    -- * H265GopSizeUnits
    , H265GopSizeUnits (..)

    -- * H265Level
    , H265Level (..)

    -- * H265LookAheadRateControl
    , H265LookAheadRateControl (..)

    -- * H265Profile
    , H265Profile (..)

    -- * H265RateControlMode
    , H265RateControlMode (..)

    -- * H265ScanType
    , H265ScanType (..)

    -- * H265SceneChangeDetect
    , H265SceneChangeDetect (..)

    -- * H265Tier
    , H265Tier (..)

    -- * H265TimecodeInsertionBehavior
    , H265TimecodeInsertionBehavior (..)

    -- * HlsAdMarkers
    , HlsAdMarkers (..)

    -- * HlsAkamaiHTTPTransferMode
    , HlsAkamaiHTTPTransferMode (..)

    -- * HlsCaptionLanguageSetting
    , HlsCaptionLanguageSetting (..)

    -- * HlsClientCache
    , HlsClientCache (..)

    -- * HlsCodecSpecification
    , HlsCodecSpecification (..)

    -- * HlsDirectoryStructure
    , HlsDirectoryStructure (..)

    -- * HlsEncryptionType
    , HlsEncryptionType (..)

    -- * HlsH265PackagingType
    , HlsH265PackagingType (..)

    -- * HlsId3SegmentTaggingState
    , HlsId3SegmentTaggingState (..)

    -- * HlsIvInManifest
    , HlsIvInManifest (..)

    -- * HlsIvSource
    , HlsIvSource (..)

    -- * HlsManifestCompression
    , HlsManifestCompression (..)

    -- * HlsManifestDurationFormat
    , HlsManifestDurationFormat (..)

    -- * HlsMediaStoreStorageClass
    , HlsMediaStoreStorageClass (..)

    -- * HlsMode
    , HlsMode (..)

    -- * HlsOutputSelection
    , HlsOutputSelection (..)

    -- * HlsProgramDateTime
    , HlsProgramDateTime (..)

    -- * HlsRedundantManifest
    , HlsRedundantManifest (..)

    -- * HlsSegmentationMode
    , HlsSegmentationMode (..)

    -- * HlsStreamInfResolution
    , HlsStreamInfResolution (..)

    -- * HlsTimedMetadataId3Frame
    , HlsTimedMetadataId3Frame (..)

    -- * HlsTsFileMode
    , HlsTsFileMode (..)

    -- * HlsWebdavHTTPTransferMode
    , HlsWebdavHTTPTransferMode (..)

    -- * IFrameOnlyPlaylistType
    , IFrameOnlyPlaylistType (..)

    -- * InputClass
    , InputClass (..)

    -- * InputCodec
    , InputCodec (..)

    -- * InputDeblockFilter
    , InputDeblockFilter (..)

    -- * InputDenoiseFilter
    , InputDenoiseFilter (..)

    -- * InputDeviceActiveInput
    , InputDeviceActiveInput (..)

    -- * InputDeviceConfiguredInput
    , InputDeviceConfiguredInput (..)

    -- * InputDeviceConnectionState
    , InputDeviceConnectionState (..)

    -- * InputDeviceIPScheme
    , InputDeviceIPScheme (..)

    -- * InputDeviceScanType
    , InputDeviceScanType (..)

    -- * InputDeviceState
    , InputDeviceState (..)

    -- * InputDeviceType
    , InputDeviceType (..)

    -- * InputFilter
    , InputFilter (..)

    -- * InputLossActionForHlsOut
    , InputLossActionForHlsOut (..)

    -- * InputLossActionForMsSmoothOut
    , InputLossActionForMsSmoothOut (..)

    -- * InputLossActionForRtmpOut
    , InputLossActionForRtmpOut (..)

    -- * InputLossActionForUdpOut
    , InputLossActionForUdpOut (..)

    -- * InputLossImageType
    , InputLossImageType (..)

    -- * InputMaximumBitrate
    , InputMaximumBitrate (..)

    -- * InputPreference
    , InputPreference (..)

    -- * InputResolution
    , InputResolution (..)

    -- * InputSecurityGroupState
    , InputSecurityGroupState (..)

    -- * InputSourceEndBehavior
    , InputSourceEndBehavior (..)

    -- * InputSourceType
    , InputSourceType (..)

    -- * InputState
    , InputState (..)

    -- * InputTimecodeSource
    , InputTimecodeSource (..)

    -- * InputType
    , InputType (..)

    -- * LastFrameClippingBehavior
    , LastFrameClippingBehavior (..)

    -- * LogLevel
    , LogLevel (..)

    -- * M2tsAbsentInputAudioBehavior
    , M2tsAbsentInputAudioBehavior (..)

    -- * M2tsArib
    , M2tsArib (..)

    -- * M2tsAribCaptionsPidControl
    , M2tsAribCaptionsPidControl (..)

    -- * M2tsAudioBufferModel
    , M2tsAudioBufferModel (..)

    -- * M2tsAudioInterval
    , M2tsAudioInterval (..)

    -- * M2tsAudioStreamType
    , M2tsAudioStreamType (..)

    -- * M2tsBufferModel
    , M2tsBufferModel (..)

    -- * M2tsCCDescriptor
    , M2tsCCDescriptor (..)

    -- * M2tsEbifControl
    , M2tsEbifControl (..)

    -- * M2tsEbpPlacement
    , M2tsEbpPlacement (..)

    -- * M2tsEsRateInPes
    , M2tsEsRateInPes (..)

    -- * M2tsKlv
    , M2tsKlv (..)

    -- * M2tsNielsenId3Behavior
    , M2tsNielsenId3Behavior (..)

    -- * M2tsPcrControl
    , M2tsPcrControl (..)

    -- * M2tsRateMode
    , M2tsRateMode (..)

    -- * M2tsScte35Control
    , M2tsScte35Control (..)

    -- * M2tsSegmentationMarkers
    , M2tsSegmentationMarkers (..)

    -- * M2tsSegmentationStyle
    , M2tsSegmentationStyle (..)

    -- * M2tsTimedMetadataBehavior
    , M2tsTimedMetadataBehavior (..)

    -- * M3u8NielsenId3Behavior
    , M3u8NielsenId3Behavior (..)

    -- * M3u8PcrControl
    , M3u8PcrControl (..)

    -- * M3u8Scte35Behavior
    , M3u8Scte35Behavior (..)

    -- * M3u8TimedMetadataBehavior
    , M3u8TimedMetadataBehavior (..)

    -- * Mp2CodingMode
    , Mp2CodingMode (..)

    -- * MsSmoothH265PackagingType
    , MsSmoothH265PackagingType (..)

    -- * MultiplexState
    , MultiplexState (..)

    -- * NetworkInputServerValidation
    , NetworkInputServerValidation (..)

    -- * NielsenPcmToId3TaggingState
    , NielsenPcmToId3TaggingState (..)

    -- * OfferingDurationUnits
    , OfferingDurationUnits (..)

    -- * OfferingType
    , OfferingType (..)

    -- * PipelineId
    , PipelineId (..)

    -- * PreferredChannelPipeline
    , PreferredChannelPipeline (..)

    -- * ReservationCodec
    , ReservationCodec (..)

    -- * ReservationMaximumBitrate
    , ReservationMaximumBitrate (..)

    -- * ReservationMaximumFramerate
    , ReservationMaximumFramerate (..)

    -- * ReservationResolution
    , ReservationResolution (..)

    -- * ReservationResourceType
    , ReservationResourceType (..)

    -- * ReservationSpecialFeature
    , ReservationSpecialFeature (..)

    -- * ReservationState
    , ReservationState (..)

    -- * ReservationVideoQuality
    , ReservationVideoQuality (..)

    -- * RtmpCacheFullBehavior
    , RtmpCacheFullBehavior (..)

    -- * RtmpCaptionData
    , RtmpCaptionData (..)

    -- * RtmpOutputCertificateMode
    , RtmpOutputCertificateMode (..)

    -- * Scte20Convert608To708
    , Scte20Convert608To708 (..)

    -- * Scte35AposNoRegionalBlackoutBehavior
    , Scte35AposNoRegionalBlackoutBehavior (..)

    -- * Scte35AposWebDeliveryAllowedBehavior
    , Scte35AposWebDeliveryAllowedBehavior (..)

    -- * Scte35ArchiveAllowedFlag
    , Scte35ArchiveAllowedFlag (..)

    -- * Scte35DeviceRestrictions
    , Scte35DeviceRestrictions (..)

    -- * Scte35NoRegionalBlackoutFlag
    , Scte35NoRegionalBlackoutFlag (..)

    -- * Scte35SegmentationCancelIndicator
    , Scte35SegmentationCancelIndicator (..)

    -- * Scte35SpliceInsertNoRegionalBlackoutBehavior
    , Scte35SpliceInsertNoRegionalBlackoutBehavior (..)

    -- * Scte35SpliceInsertWebDeliveryAllowedBehavior
    , Scte35SpliceInsertWebDeliveryAllowedBehavior (..)

    -- * Scte35WebDeliveryAllowedFlag
    , Scte35WebDeliveryAllowedFlag (..)

    -- * SmoothGroupAudioOnlyTimecodeControl
    , SmoothGroupAudioOnlyTimecodeControl (..)

    -- * SmoothGroupCertificateMode
    , SmoothGroupCertificateMode (..)

    -- * SmoothGroupEventIdMode
    , SmoothGroupEventIdMode (..)

    -- * SmoothGroupEventStopBehavior
    , SmoothGroupEventStopBehavior (..)

    -- * SmoothGroupSegmentationMode
    , SmoothGroupSegmentationMode (..)

    -- * SmoothGroupSparseTrackType
    , SmoothGroupSparseTrackType (..)

    -- * SmoothGroupStreamManifestBehavior
    , SmoothGroupStreamManifestBehavior (..)

    -- * SmoothGroupTimestampOffsetMode
    , SmoothGroupTimestampOffsetMode (..)

    -- * Smpte2038DataPreference
    , Smpte2038DataPreference (..)

    -- * TemporalFilterPostFilterSharpening
    , TemporalFilterPostFilterSharpening (..)

    -- * TemporalFilterStrength
    , TemporalFilterStrength (..)

    -- * TimecodeConfigSource
    , TimecodeConfigSource (..)

    -- * TtmlDestinationStyleControl
    , TtmlDestinationStyleControl (..)

    -- * UdpTimedMetadataId3Frame
    , UdpTimedMetadataId3Frame (..)

    -- * VideoDescriptionRespondToAfd
    , VideoDescriptionRespondToAfd (..)

    -- * VideoDescriptionScalingBehavior
    , VideoDescriptionScalingBehavior (..)

    -- * VideoSelectorColorSpace
    , VideoSelectorColorSpace (..)

    -- * VideoSelectorColorSpaceUsage
    , VideoSelectorColorSpaceUsage (..)

    -- * AacSettings
    , AacSettings
    , aacSettings
    , aRawFormat
    , aCodingMode
    , aProfile
    , aRateControlMode
    , aSampleRate
    , aSpec
    , aBitrate
    , aVbrQuality
    , aInputType

    -- * Ac3Settings
    , Ac3Settings
    , ac3Settings
    , asLfeFilter
    , asMetadataControl
    , asBitstreamMode
    , asCodingMode
    , asBitrate
    , asDialnorm
    , asDrcProfile

    -- * ArchiveContainerSettings
    , ArchiveContainerSettings
    , archiveContainerSettings
    , acsM2tsSettings

    -- * ArchiveGroupSettings
    , ArchiveGroupSettings
    , archiveGroupSettings
    , agsRolloverInterval
    , agsDestination

    -- * ArchiveOutputSettings
    , ArchiveOutputSettings
    , archiveOutputSettings
    , aosExtension
    , aosNameModifier
    , aosContainerSettings

    -- * AribDestinationSettings
    , AribDestinationSettings
    , aribDestinationSettings

    -- * AribSourceSettings
    , AribSourceSettings
    , aribSourceSettings

    -- * AudioChannelMapping
    , AudioChannelMapping
    , audioChannelMapping
    , acmOutputChannel
    , acmInputChannelLevels

    -- * AudioCodecSettings
    , AudioCodecSettings
    , audioCodecSettings
    , acsPassThroughSettings
    , acsAc3Settings
    , acsMp2Settings
    , acsAacSettings
    , acsEac3Settings

    -- * AudioDescription
    , AudioDescription
    , audioDescription
    , adLanguageCode
    , adAudioType
    , adAudioNormalizationSettings
    , adLanguageCodeControl
    , adCodecSettings
    , adStreamName
    , adRemixSettings
    , adAudioTypeControl
    , adAudioSelectorName
    , adName

    -- * AudioLanguageSelection
    , AudioLanguageSelection
    , audioLanguageSelection
    , alsLanguageSelectionPolicy
    , alsLanguageCode

    -- * AudioNormalizationSettings
    , AudioNormalizationSettings
    , audioNormalizationSettings
    , ansAlgorithmControl
    , ansTargetLkfs
    , ansAlgorithm

    -- * AudioOnlyHlsSettings
    , AudioOnlyHlsSettings
    , audioOnlyHlsSettings
    , aohsAudioOnlyImage
    , aohsSegmentType
    , aohsAudioGroupId
    , aohsAudioTrackType

    -- * AudioPidSelection
    , AudioPidSelection
    , audioPidSelection
    , apsPid

    -- * AudioSelector
    , AudioSelector
    , audioSelector
    , asSelectorSettings
    , asName

    -- * AudioSelectorSettings
    , AudioSelectorSettings
    , audioSelectorSettings
    , assAudioLanguageSelection
    , assAudioTrackSelection
    , assAudioPidSelection

    -- * AudioTrack
    , AudioTrack
    , audioTrack
    , atTrack

    -- * AudioTrackSelection
    , AudioTrackSelection
    , audioTrackSelection
    , atsTracks

    -- * AutomaticInputFailoverSettings
    , AutomaticInputFailoverSettings
    , automaticInputFailoverSettings
    , aifsInputPreference
    , aifsSecondaryInputId

    -- * AvailBlanking
    , AvailBlanking
    , availBlanking
    , abState
    , abAvailBlankingImage

    -- * AvailConfiguration
    , AvailConfiguration
    , availConfiguration
    , acAvailSettings

    -- * AvailSettings
    , AvailSettings
    , availSettings
    , asScte35SpliceInsert
    , asScte35TimeSignalApos

    -- * BatchScheduleActionCreateRequest
    , BatchScheduleActionCreateRequest
    , batchScheduleActionCreateRequest
    , bsacrScheduleActions

    -- * BatchScheduleActionCreateResult
    , BatchScheduleActionCreateResult
    , batchScheduleActionCreateResult
    , bScheduleActions

    -- * BatchScheduleActionDeleteRequest
    , BatchScheduleActionDeleteRequest
    , batchScheduleActionDeleteRequest
    , bsadrActionNames

    -- * BatchScheduleActionDeleteResult
    , BatchScheduleActionDeleteResult
    , batchScheduleActionDeleteResult
    , bsadrScheduleActions

    -- * BlackoutSlate
    , BlackoutSlate
    , blackoutSlate
    , bsNetworkEndBlackoutImage
    , bsState
    , bsNetworkEndBlackout
    , bsNetworkId
    , bsBlackoutSlateImage

    -- * BurnInDestinationSettings
    , BurnInDestinationSettings
    , burnInDestinationSettings
    , bidsBackgroundOpacity
    , bidsFontOpacity
    , bidsShadowYOffset
    , bidsFontResolution
    , bidsYPosition
    , bidsBackgroundColor
    , bidsShadowXOffset
    , bidsFontSize
    , bidsXPosition
    , bidsAlignment
    , bidsShadowOpacity
    , bidsTeletextGridControl
    , bidsOutlineColor
    , bidsOutlineSize
    , bidsFont
    , bidsShadowColor
    , bidsFontColor

    -- * CaptionDescription
    , CaptionDescription
    , captionDescription
    , cdLanguageCode
    , cdDestinationSettings
    , cdLanguageDescription
    , cdCaptionSelectorName
    , cdName

    -- * CaptionDestinationSettings
    , CaptionDestinationSettings
    , captionDestinationSettings
    , cdsTeletextDestinationSettings
    , cdsRtmpCaptionInfoDestinationSettings
    , cdsDvbSubDestinationSettings
    , cdsScte27DestinationSettings
    , cdsTtmlDestinationSettings
    , cdsScte20PlusEmbeddedDestinationSettings
    , cdsEmbeddedPlusScte20DestinationSettings
    , cdsSmpteTtDestinationSettings
    , cdsWebvttDestinationSettings
    , cdsEmbeddedDestinationSettings
    , cdsBurnInDestinationSettings
    , cdsAribDestinationSettings

    -- * CaptionLanguageMapping
    , CaptionLanguageMapping
    , captionLanguageMapping
    , clmLanguageCode
    , clmLanguageDescription
    , clmCaptionChannel

    -- * CaptionSelector
    , CaptionSelector
    , captionSelector
    , csLanguageCode
    , csSelectorSettings
    , csName

    -- * CaptionSelectorSettings
    , CaptionSelectorSettings
    , captionSelectorSettings
    , cssTeletextSourceSettings
    , cssAribSourceSettings
    , cssScte27SourceSettings
    , cssDvbSubSourceSettings
    , cssScte20SourceSettings
    , cssEmbeddedSourceSettings

    -- * Channel
    , Channel
    , channel
    , cState
    , cLogLevel
    , cARN
    , cPipelinesRunningCount
    , cPipelineDetails
    , cInputSpecification
    , cInputAttachments
    , cDestinations
    , cName
    , cId
    , cChannelClass
    , cEgressEndpoints
    , cTags
    , cEncoderSettings
    , cRoleARN

    -- * ChannelEgressEndpoint
    , ChannelEgressEndpoint
    , channelEgressEndpoint
    , ceeSourceIP

    -- * ChannelSummary
    , ChannelSummary
    , channelSummary
    , chaState
    , chaLogLevel
    , chaARN
    , chaPipelinesRunningCount
    , chaInputSpecification
    , chaInputAttachments
    , chaDestinations
    , chaName
    , chaId
    , chaChannelClass
    , chaEgressEndpoints
    , chaTags
    , chaRoleARN

    -- * ColorSpacePassthroughSettings
    , ColorSpacePassthroughSettings
    , colorSpacePassthroughSettings

    -- * DvbNitSettings
    , DvbNitSettings
    , dvbNitSettings
    , dnsRepInterval
    , dnsNetworkName
    , dnsNetworkId

    -- * DvbSdtSettings
    , DvbSdtSettings
    , dvbSdtSettings
    , dssRepInterval
    , dssServiceProviderName
    , dssOutputSdt
    , dssServiceName

    -- * DvbSubDestinationSettings
    , DvbSubDestinationSettings
    , dvbSubDestinationSettings
    , dsdsBackgroundOpacity
    , dsdsFontOpacity
    , dsdsShadowYOffset
    , dsdsFontResolution
    , dsdsYPosition
    , dsdsBackgroundColor
    , dsdsShadowXOffset
    , dsdsFontSize
    , dsdsXPosition
    , dsdsAlignment
    , dsdsShadowOpacity
    , dsdsTeletextGridControl
    , dsdsOutlineColor
    , dsdsOutlineSize
    , dsdsFont
    , dsdsShadowColor
    , dsdsFontColor

    -- * DvbSubSourceSettings
    , DvbSubSourceSettings
    , dvbSubSourceSettings
    , dsssPid

    -- * DvbTdtSettings
    , DvbTdtSettings
    , dvbTdtSettings
    , dtsRepInterval

    -- * Eac3Settings
    , Eac3Settings
    , eac3Settings
    , esStereoDownmix
    , esLoRoCenterMixLevel
    , esLtRtCenterMixLevel
    , esLfeFilter
    , esLtRtSurroundMixLevel
    , esMetadataControl
    , esLoRoSurroundMixLevel
    , esSurroundMode
    , esAttenuationControl
    , esPassthroughControl
    , esBitstreamMode
    , esLfeControl
    , esCodingMode
    , esDrcLine
    , esDrcRf
    , esDcFilter
    , esBitrate
    , esPhaseControl
    , esSurroundExMode
    , esDialnorm

    -- * EmbeddedDestinationSettings
    , EmbeddedDestinationSettings
    , embeddedDestinationSettings

    -- * EmbeddedPlusScte20DestinationSettings
    , EmbeddedPlusScte20DestinationSettings
    , embeddedPlusScte20DestinationSettings

    -- * EmbeddedSourceSettings
    , EmbeddedSourceSettings
    , embeddedSourceSettings
    , essConvert608To708
    , essScte20Detection
    , essSource608TrackNumber
    , essSource608ChannelNumber

    -- * EncoderSettings
    , EncoderSettings
    , encoderSettings
    , esCaptionDescriptions
    , esAvailConfiguration
    , esFeatureActivations
    , esNielsenConfiguration
    , esAvailBlanking
    , esGlobalConfiguration
    , esBlackoutSlate
    , esVideoDescriptions
    , esAudioDescriptions
    , esOutputGroups
    , esTimecodeConfig

    -- * FeatureActivations
    , FeatureActivations
    , featureActivations
    , faInputPrepareScheduleActions

    -- * FecOutputSettings
    , FecOutputSettings
    , fecOutputSettings
    , fosRowLength
    , fosIncludeFec
    , fosColumnDepth

    -- * FixedModeScheduleActionStartSettings
    , FixedModeScheduleActionStartSettings
    , fixedModeScheduleActionStartSettings
    , fmsassTime

    -- * Fmp4HlsSettings
    , Fmp4HlsSettings
    , fmp4HlsSettings
    , fhsNielsenId3Behavior
    , fhsAudioRenditionSets
    , fhsTimedMetadataBehavior

    -- * FollowModeScheduleActionStartSettings
    , FollowModeScheduleActionStartSettings
    , followModeScheduleActionStartSettings
    , fmsassReferenceActionName
    , fmsassFollowPoint

    -- * FrameCaptureGroupSettings
    , FrameCaptureGroupSettings
    , frameCaptureGroupSettings
    , fcgsDestination

    -- * FrameCaptureOutputSettings
    , FrameCaptureOutputSettings
    , frameCaptureOutputSettings
    , fcosNameModifier

    -- * FrameCaptureSettings
    , FrameCaptureSettings
    , frameCaptureSettings
    , fcsCaptureIntervalUnits
    , fcsCaptureInterval

    -- * GlobalConfiguration
    , GlobalConfiguration
    , globalConfiguration
    , gcOutputLockingMode
    , gcInputLossBehavior
    , gcInitialAudioGain
    , gcSupportLowFramerateInputs
    , gcInputEndAction
    , gcOutputTimingSource

    -- * H264ColorSpaceSettings
    , H264ColorSpaceSettings
    , h264ColorSpaceSettings
    , hRec709Settings
    , hRec601Settings
    , hColorSpacePassthroughSettings

    -- * H264FilterSettings
    , H264FilterSettings
    , h264FilterSettings
    , hfsTemporalFilterSettings

    -- * H264Settings
    , H264Settings
    , h264Settings
    , hssTemporalAq
    , hssSceneChangeDetect
    , hssScanType
    , hssTimecodeInsertion
    , hssParNumerator
    , hssAfdSignaling
    , hssGopSize
    , hssGopSizeUnits
    , hssSubgopLength
    , hssQualityLevel
    , hssSlices
    , hssProfile
    , hssRateControlMode
    , hssMinIInterval
    , hssQvbrQualityLevel
    , hssColorSpaceSettings
    , hssParControl
    , hssFlickerAq
    , hssBufSize
    , hssSpatialAq
    , hssGopNumBFrames
    , hssFixedAfd
    , hssSoftness
    , hssFilterSettings
    , hssBitrate
    , hssFramerateDenominator
    , hssForceFieldPictures
    , hssEntropyEncoding
    , hssFramerateControl
    , hssColorMetadata
    , hssLookAheadRateControl
    , hssAdaptiveQuantization
    , hssFramerateNumerator
    , hssLevel
    , hssGopBReference
    , hssMaxBitrate
    , hssSyntax
    , hssBufFillPct
    , hssGopClosedCadence
    , hssNumRefFrames
    , hssParDenominator

    -- * H265ColorSpaceSettings
    , H265ColorSpaceSettings
    , h265ColorSpaceSettings
    , hcssHdr10Settings
    , hcssRec709Settings
    , hcssRec601Settings
    , hcssColorSpacePassthroughSettings

    -- * H265Settings
    , H265Settings
    , h265Settings
    , hsSceneChangeDetect
    , hsScanType
    , hsTimecodeInsertion
    , hsParNumerator
    , hsAfdSignaling
    , hsGopSize
    , hsGopSizeUnits
    , hsSlices
    , hsProfile
    , hsAlternativeTransferFunction
    , hsRateControlMode
    , hsMinIInterval
    , hsQvbrQualityLevel
    , hsColorSpaceSettings
    , hsFlickerAq
    , hsBufSize
    , hsTier
    , hsFixedAfd
    , hsBitrate
    , hsColorMetadata
    , hsLookAheadRateControl
    , hsAdaptiveQuantization
    , hsLevel
    , hsMaxBitrate
    , hsGopClosedCadence
    , hsParDenominator
    , hsFramerateNumerator
    , hsFramerateDenominator

    -- * Hdr10Settings
    , Hdr10Settings
    , hdr10Settings
    , hsMaxFall
    , hsMaxCll

    -- * HlsAkamaiSettings
    , HlsAkamaiSettings
    , hlsAkamaiSettings
    , hasHTTPTransferMode
    , hasNumRetries
    , hasToken
    , hasConnectionRetryInterval
    , hasFilecacheDuration
    , hasRestartDelay
    , hasSalt

    -- * HlsBasicPutSettings
    , HlsBasicPutSettings
    , hlsBasicPutSettings
    , hbpsNumRetries
    , hbpsConnectionRetryInterval
    , hbpsFilecacheDuration
    , hbpsRestartDelay

    -- * HlsCdnSettings
    , HlsCdnSettings
    , hlsCdnSettings
    , hcsHlsAkamaiSettings
    , hcsHlsMediaStoreSettings
    , hcsHlsBasicPutSettings
    , hcsHlsWebdavSettings

    -- * HlsGroupSettings
    , HlsGroupSettings
    , hlsGroupSettings
    , hgsDirectoryStructure
    , hgsEncryptionType
    , hgsTimedMetadataId3Period
    , hgsIvInManifest
    , hgsTsFileMode
    , hgsMinSegmentLength
    , hgsIFrameOnlyPlaylists
    , hgsProgramDateTime
    , hgsIndexNSegments
    , hgsProgramDateTimePeriod
    , hgsCodecSpecification
    , hgsHlsCdnSettings
    , hgsCaptionLanguageMappings
    , hgsInputLossAction
    , hgsMode
    , hgsKeyProviderSettings
    , hgsConstantIv
    , hgsBaseURLManifest
    , hgsAdMarkers
    , hgsKeyFormat
    , hgsSegmentLength
    , hgsHlsId3SegmentTagging
    , hgsTimedMetadataId3Frame
    , hgsBaseURLContent
    , hgsOutputSelection
    , hgsCaptionLanguageSetting
    , hgsSegmentsPerSubdirectory
    , hgsManifestDurationFormat
    , hgsIvSource
    , hgsSegmentationMode
    , hgsKeyFormatVersions
    , hgsClientCache
    , hgsTimestampDeltaMilliseconds
    , hgsBaseURLManifest1
    , hgsRedundantManifest
    , hgsStreamInfResolution
    , hgsKeepSegments
    , hgsBaseURLContent1
    , hgsManifestCompression
    , hgsDestination

    -- * HlsId3SegmentTaggingScheduleActionSettings
    , HlsId3SegmentTaggingScheduleActionSettings
    , hlsId3SegmentTaggingScheduleActionSettings
    , histsasTag

    -- * HlsInputSettings
    , HlsInputSettings
    , hlsInputSettings
    , hisBufferSegments
    , hisRetries
    , hisRetryInterval
    , hisBandwidth

    -- * HlsMediaStoreSettings
    , HlsMediaStoreSettings
    , hlsMediaStoreSettings
    , hmssNumRetries
    , hmssConnectionRetryInterval
    , hmssFilecacheDuration
    , hmssMediaStoreStorageClass
    , hmssRestartDelay

    -- * HlsOutputSettings
    , HlsOutputSettings
    , hlsOutputSettings
    , hosH265PackagingType
    , hosSegmentModifier
    , hosNameModifier
    , hosHlsSettings

    -- * HlsSettings
    , HlsSettings
    , hlsSettings
    , hsFmp4HlsSettings
    , hsAudioOnlyHlsSettings
    , hsStandardHlsSettings

    -- * HlsTimedMetadataScheduleActionSettings
    , HlsTimedMetadataScheduleActionSettings
    , hlsTimedMetadataScheduleActionSettings
    , htmsasId3

    -- * HlsWebdavSettings
    , HlsWebdavSettings
    , hlsWebdavSettings
    , hwsHTTPTransferMode
    , hwsNumRetries
    , hwsConnectionRetryInterval
    , hwsFilecacheDuration
    , hwsRestartDelay

    -- * ImmediateModeScheduleActionStartSettings
    , ImmediateModeScheduleActionStartSettings
    , immediateModeScheduleActionStartSettings

    -- * Input
    , Input
    , input
    , iState
    , iSecurityGroups
    , iARN
    , iInputDevices
    , iSources
    , iDestinations
    , iName
    , iAttachedChannels
    , iId
    , iInputClass
    , iType
    , iMediaConnectFlows
    , iInputSourceType
    , iTags
    , iRoleARN

    -- * InputAttachment
    , InputAttachment
    , inputAttachment
    , iaInputAttachmentName
    , iaInputId
    , iaAutomaticInputFailoverSettings
    , iaInputSettings

    -- * InputChannelLevel
    , InputChannelLevel
    , inputChannelLevel
    , iclInputChannel
    , iclGain

    -- * InputClippingSettings
    , InputClippingSettings
    , inputClippingSettings
    , icsStopTimecode
    , icsStartTimecode
    , icsInputTimecodeSource

    -- * InputDestination
    , InputDestination
    , inputDestination
    , idURL
    , idIP
    , idVPC
    , idPort

    -- * InputDestinationRequest
    , InputDestinationRequest
    , inputDestinationRequest
    , idrStreamName

    -- * InputDestinationVPC
    , InputDestinationVPC
    , inputDestinationVPC
    , idvNetworkInterfaceId
    , idvAvailabilityZone

    -- * InputDeviceConfigurableSettings
    , InputDeviceConfigurableSettings
    , inputDeviceConfigurableSettings
    , idcsConfiguredInput
    , idcsMaxBitrate

    -- * InputDeviceHdSettings
    , InputDeviceHdSettings
    , inputDeviceHdSettings
    , idhsFramerate
    , idhsScanType
    , idhsDeviceState
    , idhsHeight
    , idhsActiveInput
    , idhsWidth
    , idhsConfiguredInput
    , idhsMaxBitrate

    -- * InputDeviceNetworkSettings
    , InputDeviceNetworkSettings
    , inputDeviceNetworkSettings
    , idnsIPAddress
    , idnsGateway
    , idnsDNSAddresses
    , idnsIPScheme
    , idnsSubnetMask

    -- * InputDeviceRequest
    , InputDeviceRequest
    , inputDeviceRequest
    , idrId

    -- * InputDeviceSettings
    , InputDeviceSettings
    , inputDeviceSettings
    , idssId

    -- * InputDeviceSummary
    , InputDeviceSummary
    , inputDeviceSummary
    , idsARN
    , idsMACAddress
    , idsHdDeviceSettings
    , idsName
    , idsId
    , idsDeviceSettingsSyncState
    , idsType
    , idsSerialNumber
    , idsNetworkSettings
    , idsConnectionState

    -- * InputLocation
    , InputLocation
    , inputLocation
    , ilUsername
    , ilPasswordParam
    , ilURI

    -- * InputLossBehavior
    , InputLossBehavior
    , inputLossBehavior
    , ilbInputLossImageColor
    , ilbBlackFrameMsec
    , ilbRepeatFrameMsec
    , ilbInputLossImageType
    , ilbInputLossImageSlate

    -- * InputPrepareScheduleActionSettings
    , InputPrepareScheduleActionSettings
    , inputPrepareScheduleActionSettings
    , ipsasInputClippingSettings
    , ipsasURLPath
    , ipsasInputAttachmentNameReference

    -- * InputSecurityGroup
    , InputSecurityGroup
    , inputSecurityGroup
    , isgState
    , isgARN
    , isgInputs
    , isgId
    , isgWhitelistRules
    , isgTags

    -- * InputSettings
    , InputSettings
    , inputSettings
    , isVideoSelector
    , isSmpte2038DataPreference
    , isNetworkInputSettings
    , isAudioSelectors
    , isDeblockFilter
    , isDenoiseFilter
    , isFilterStrength
    , isCaptionSelectors
    , isInputFilter
    , isSourceEndBehavior

    -- * InputSource
    , InputSource
    , inputSource
    , isURL
    , isUsername
    , isPasswordParam

    -- * InputSourceRequest
    , InputSourceRequest
    , inputSourceRequest
    , isrURL
    , isrUsername
    , isrPasswordParam

    -- * InputSpecification
    , InputSpecification
    , inputSpecification
    , isResolution
    , isCodec
    , isMaximumBitrate

    -- * InputSwitchScheduleActionSettings
    , InputSwitchScheduleActionSettings
    , inputSwitchScheduleActionSettings
    , issasInputClippingSettings
    , issasURLPath
    , issasInputAttachmentNameReference

    -- * InputVPCRequest
    , InputVPCRequest
    , inputVPCRequest
    , ivrSecurityGroupIds
    , ivrSubnetIds

    -- * InputWhitelistRule
    , InputWhitelistRule
    , inputWhitelistRule
    , iwrCidr

    -- * InputWhitelistRuleCidr
    , InputWhitelistRuleCidr
    , inputWhitelistRuleCidr
    , iwrcCidr

    -- * KeyProviderSettings
    , KeyProviderSettings
    , keyProviderSettings
    , kpsStaticKeySettings

    -- * M2tsSettings
    , M2tsSettings
    , m2tsSettings
    , msPmtPid
    , msEtvSignalPid
    , msVideoPid
    , msNielsenId3Behavior
    , msBufferModel
    , msScte35Pid
    , msTransportStreamId
    , msProgramNum
    , msFragmentTime
    , msTimedMetadataBehavior
    , msCCDescriptor
    , msPmtInterval
    , msDvbSdtSettings
    , msEcmPid
    , msNullPacketBitrate
    , msAudioBufferModel
    , msTimedMetadataPid
    , msKlv
    , msAudioFramesPerPes
    , msPcrPeriod
    , msPcrPid
    , msSegmentationMarkers
    , msAribCaptionsPidControl
    , msKlvDataPids
    , msEbpLookaheadMs
    , msDvbSubPids
    , msScte27Pids
    , msPatInterval
    , msAudioStreamType
    , msEsRateInPes
    , msEtvPlatformPid
    , msBitrate
    , msScte35Control
    , msAudioPids
    , msDvbTeletextPid
    , msEbif
    , msArib
    , msAribCaptionsPid
    , msAbsentInputAudioBehavior
    , msSegmentationTime
    , msEbpAudioInterval
    , msDvbNitSettings
    , msPcrControl
    , msEbpPlacement
    , msRateMode
    , msSegmentationStyle
    , msDvbTdtSettings

    -- * M3u8Settings
    , M3u8Settings
    , m3u8Settings
    , mPmtPid
    , mVideoPid
    , mNielsenId3Behavior
    , mScte35Pid
    , mTransportStreamId
    , mProgramNum
    , mTimedMetadataBehavior
    , mPmtInterval
    , mEcmPid
    , mTimedMetadataPid
    , mAudioFramesPerPes
    , mPcrPeriod
    , mPcrPid
    , mPatInterval
    , mAudioPids
    , mScte35Behavior
    , mPcrControl

    -- * MediaConnectFlow
    , MediaConnectFlow
    , mediaConnectFlow
    , mcfFlowARN

    -- * MediaConnectFlowRequest
    , MediaConnectFlowRequest
    , mediaConnectFlowRequest
    , mcfrFlowARN

    -- * MediaPackageGroupSettings
    , MediaPackageGroupSettings
    , mediaPackageGroupSettings
    , mpgsDestination

    -- * MediaPackageOutputDestinationSettings
    , MediaPackageOutputDestinationSettings
    , mediaPackageOutputDestinationSettings
    , mpodsChannelId

    -- * MediaPackageOutputSettings
    , MediaPackageOutputSettings
    , mediaPackageOutputSettings

    -- * Mp2Settings
    , Mp2Settings
    , mp2Settings
    , mCodingMode
    , mSampleRate
    , mBitrate

    -- * MsSmoothGroupSettings
    , MsSmoothGroupSettings
    , msSmoothGroupSettings
    , msgsFragmentLength
    , msgsStreamManifestBehavior
    , msgsSendDelayMs
    , msgsEventStopBehavior
    , msgsTimestampOffsetMode
    , msgsNumRetries
    , msgsAcquisitionPointId
    , msgsInputLossAction
    , msgsTimestampOffset
    , msgsCertificateMode
    , msgsSparseTrackType
    , msgsConnectionRetryInterval
    , msgsFilecacheDuration
    , msgsRestartDelay
    , msgsEventIdMode
    , msgsAudioOnlyTimecodeControl
    , msgsSegmentationMode
    , msgsEventId
    , msgsDestination

    -- * MsSmoothOutputSettings
    , MsSmoothOutputSettings
    , msSmoothOutputSettings
    , msosH265PackagingType
    , msosNameModifier

    -- * Multiplex
    , Multiplex
    , multiplex
    , mState
    , mARN
    , mPipelinesRunningCount
    , mAvailabilityZones
    , mProgramCount
    , mDestinations
    , mName
    , mId
    , mMultiplexSettings
    , mTags

    -- * MultiplexGroupSettings
    , MultiplexGroupSettings
    , multiplexGroupSettings

    -- * MultiplexMediaConnectOutputDestinationSettings
    , MultiplexMediaConnectOutputDestinationSettings
    , multiplexMediaConnectOutputDestinationSettings
    , mmcodsEntitlementARN

    -- * MultiplexOutputDestination
    , MultiplexOutputDestination
    , multiplexOutputDestination
    , modMediaConnectSettings

    -- * MultiplexOutputSettings
    , MultiplexOutputSettings
    , multiplexOutputSettings
    , mosDestination

    -- * MultiplexProgram
    , MultiplexProgram
    , multiplexProgram
    , mpPacketIdentifiersMap
    , mpProgramName
    , mpChannelId
    , mpMultiplexProgramSettings

    -- * MultiplexProgramChannelDestinationSettings
    , MultiplexProgramChannelDestinationSettings
    , multiplexProgramChannelDestinationSettings
    , mpcdsMultiplexId
    , mpcdsProgramName

    -- * MultiplexProgramPacketIdentifiersMap
    , MultiplexProgramPacketIdentifiersMap
    , multiplexProgramPacketIdentifiersMap
    , mppimPmtPid
    , mppimEtvSignalPid
    , mppimVideoPid
    , mppimScte35Pid
    , mppimPrivateMetadataPid
    , mppimTimedMetadataPid
    , mppimPcrPid
    , mppimKlvDataPids
    , mppimDvbSubPids
    , mppimScte27Pids
    , mppimEtvPlatformPid
    , mppimAudioPids
    , mppimDvbTeletextPid

    -- * MultiplexProgramServiceDescriptor
    , MultiplexProgramServiceDescriptor
    , multiplexProgramServiceDescriptor
    , mpsdProviderName
    , mpsdServiceName

    -- * MultiplexProgramSettings
    , MultiplexProgramSettings
    , multiplexProgramSettings
    , mpsPreferredChannelPipeline
    , mpsVideoSettings
    , mpsServiceDescriptor
    , mpsProgramNumber

    -- * MultiplexProgramSummary
    , MultiplexProgramSummary
    , multiplexProgramSummary
    , mpsProgramName
    , mpsChannelId

    -- * MultiplexSettings
    , MultiplexSettings
    , multiplexSettings
    , mssMaximumVideoBufferDelayMilliseconds
    , mssTransportStreamReservedBitrate
    , mssTransportStreamBitrate
    , mssTransportStreamId

    -- * MultiplexSettingsSummary
    , MultiplexSettingsSummary
    , multiplexSettingsSummary
    , mTransportStreamBitrate

    -- * MultiplexStatmuxVideoSettings
    , MultiplexStatmuxVideoSettings
    , multiplexStatmuxVideoSettings
    , msvsMinimumBitrate
    , msvsMaximumBitrate

    -- * MultiplexSummary
    , MultiplexSummary
    , multiplexSummary
    , msState
    , msARN
    , msPipelinesRunningCount
    , msAvailabilityZones
    , msProgramCount
    , msName
    , msId
    , msMultiplexSettings
    , msTags

    -- * MultiplexVideoSettings
    , MultiplexVideoSettings
    , multiplexVideoSettings
    , mvsStatmuxSettings
    , mvsConstantBitrate

    -- * NetworkInputSettings
    , NetworkInputSettings
    , networkInputSettings
    , nisHlsInputSettings
    , nisServerValidation

    -- * NielsenConfiguration
    , NielsenConfiguration
    , nielsenConfiguration
    , ncDistributorId
    , ncNielsenPcmToId3Tagging

    -- * Offering
    , Offering
    , offering
    , oResourceSpecification
    , oCurrencyCode
    , oARN
    , oOfferingId
    , oRegion
    , oOfferingType
    , oUsagePrice
    , oFixedPrice
    , oDurationUnits
    , oOfferingDescription
    , oDuration

    -- * Output
    , Output
    , output
    , oCaptionDescriptionNames
    , oVideoDescriptionName
    , oOutputName
    , oAudioDescriptionNames
    , oOutputSettings

    -- * OutputDestination
    , OutputDestination
    , outputDestination
    , odSettings
    , odMediaPackageSettings
    , odId
    , odMultiplexSettings

    -- * OutputDestinationSettings
    , OutputDestinationSettings
    , outputDestinationSettings
    , odsURL
    , odsUsername
    , odsPasswordParam
    , odsStreamName

    -- * OutputGroup
    , OutputGroup
    , outputGroup
    , ogName
    , ogOutputs
    , ogOutputGroupSettings

    -- * OutputGroupSettings
    , OutputGroupSettings
    , outputGroupSettings
    , ogsMediaPackageGroupSettings
    , ogsMsSmoothGroupSettings
    , ogsRtmpGroupSettings
    , ogsMultiplexGroupSettings
    , ogsHlsGroupSettings
    , ogsArchiveGroupSettings
    , ogsUdpGroupSettings
    , ogsFrameCaptureGroupSettings

    -- * OutputLocationRef
    , OutputLocationRef
    , outputLocationRef
    , olrDestinationRefId

    -- * OutputSettings
    , OutputSettings
    , outputSettings
    , osMultiplexOutputSettings
    , osArchiveOutputSettings
    , osRtmpOutputSettings
    , osMediaPackageOutputSettings
    , osHlsOutputSettings
    , osFrameCaptureOutputSettings
    , osUdpOutputSettings
    , osMsSmoothOutputSettings

    -- * PassThroughSettings
    , PassThroughSettings
    , passThroughSettings

    -- * PauseStateScheduleActionSettings
    , PauseStateScheduleActionSettings
    , pauseStateScheduleActionSettings
    , pssasPipelines

    -- * PipelineDetail
    , PipelineDetail
    , pipelineDetail
    , pdPipelineId
    , pdActiveInputSwitchActionName
    , pdActiveInputAttachmentName

    -- * PipelinePauseStateSettings
    , PipelinePauseStateSettings
    , pipelinePauseStateSettings
    , ppssPipelineId

    -- * Rec601Settings
    , Rec601Settings
    , rec601Settings

    -- * Rec709Settings
    , Rec709Settings
    , rec709Settings

    -- * RemixSettings
    , RemixSettings
    , remixSettings
    , rsChannelsIn
    , rsChannelsOut
    , rsChannelMappings

    -- * Reservation
    , Reservation
    , reservation
    , rState
    , rResourceSpecification
    , rCurrencyCode
    , rARN
    , rStart
    , rCount
    , rEnd
    , rName
    , rReservationId
    , rOfferingId
    , rRegion
    , rOfferingType
    , rUsagePrice
    , rFixedPrice
    , rDurationUnits
    , rOfferingDescription
    , rDuration
    , rTags

    -- * ReservationResourceSpecification
    , ReservationResourceSpecification
    , reservationResourceSpecification
    , rrsVideoQuality
    , rrsMaximumFramerate
    , rrsResourceType
    , rrsResolution
    , rrsCodec
    , rrsSpecialFeature
    , rrsChannelClass
    , rrsMaximumBitrate

    -- * RtmpCaptionInfoDestinationSettings
    , RtmpCaptionInfoDestinationSettings
    , rtmpCaptionInfoDestinationSettings

    -- * RtmpGroupSettings
    , RtmpGroupSettings
    , rtmpGroupSettings
    , rgsInputLossAction
    , rgsCaptionData
    , rgsRestartDelay
    , rgsAuthenticationScheme
    , rgsCacheLength
    , rgsCacheFullBehavior

    -- * RtmpOutputSettings
    , RtmpOutputSettings
    , rtmpOutputSettings
    , rosNumRetries
    , rosCertificateMode
    , rosConnectionRetryInterval
    , rosDestination

    -- * ScheduleAction
    , ScheduleAction
    , scheduleAction
    , saActionName
    , saScheduleActionStartSettings
    , saScheduleActionSettings

    -- * ScheduleActionSettings
    , ScheduleActionSettings
    , scheduleActionSettings
    , sasStaticImageDeactivateSettings
    , sasScte35SpliceInsertSettings
    , sasStaticImageActivateSettings
    , sasScte35TimeSignalSettings
    , sasInputPrepareSettings
    , sasHlsId3SegmentTaggingSettings
    , sasScte35ReturnToNetworkSettings
    , sasPauseStateSettings
    , sasHlsTimedMetadataSettings
    , sasInputSwitchSettings

    -- * ScheduleActionStartSettings
    , ScheduleActionStartSettings
    , scheduleActionStartSettings
    , sassImmediateModeScheduleActionStartSettings
    , sassFollowModeScheduleActionStartSettings
    , sassFixedModeScheduleActionStartSettings

    -- * Scte20PlusEmbeddedDestinationSettings
    , Scte20PlusEmbeddedDestinationSettings
    , scte20PlusEmbeddedDestinationSettings

    -- * Scte20SourceSettings
    , Scte20SourceSettings
    , scte20SourceSettings
    , sssConvert608To708
    , sssSource608ChannelNumber

    -- * Scte27DestinationSettings
    , Scte27DestinationSettings
    , scte27DestinationSettings

    -- * Scte27SourceSettings
    , Scte27SourceSettings
    , scte27SourceSettings
    , sssPid

    -- * Scte35DeliveryRestrictions
    , Scte35DeliveryRestrictions
    , scte35DeliveryRestrictions
    , sdrDeviceRestrictions
    , sdrArchiveAllowedFlag
    , sdrWebDeliveryAllowedFlag
    , sdrNoRegionalBlackoutFlag

    -- * Scte35Descriptor
    , Scte35Descriptor
    , scte35Descriptor
    , sdScte35DescriptorSettings

    -- * Scte35DescriptorSettings
    , Scte35DescriptorSettings
    , scte35DescriptorSettings
    , sdsSegmentationDescriptorScte35DescriptorSettings

    -- * Scte35ReturnToNetworkScheduleActionSettings
    , Scte35ReturnToNetworkScheduleActionSettings
    , scte35ReturnToNetworkScheduleActionSettings
    , srtnsasSpliceEventId

    -- * Scte35SegmentationDescriptor
    , Scte35SegmentationDescriptor
    , scte35SegmentationDescriptor
    , ssdSegmentationUpidType
    , ssdSegmentsExpected
    , ssdSubSegmentsExpected
    , ssdSegmentNum
    , ssdSegmentationDuration
    , ssdSegmentationTypeId
    , ssdDeliveryRestrictions
    , ssdSegmentationUpid
    , ssdSubSegmentNum
    , ssdSegmentationEventId
    , ssdSegmentationCancelIndicator

    -- * Scte35SpliceInsert
    , Scte35SpliceInsert
    , scte35SpliceInsert
    , ssiWebDeliveryAllowedFlag
    , ssiAdAvailOffset
    , ssiNoRegionalBlackoutFlag

    -- * Scte35SpliceInsertScheduleActionSettings
    , Scte35SpliceInsertScheduleActionSettings
    , scte35SpliceInsertScheduleActionSettings
    , ssisasDuration
    , ssisasSpliceEventId

    -- * Scte35TimeSignalApos
    , Scte35TimeSignalApos
    , scte35TimeSignalApos
    , stsaWebDeliveryAllowedFlag
    , stsaAdAvailOffset
    , stsaNoRegionalBlackoutFlag

    -- * Scte35TimeSignalScheduleActionSettings
    , Scte35TimeSignalScheduleActionSettings
    , scte35TimeSignalScheduleActionSettings
    , stssasScte35Descriptors

    -- * SmpteTtDestinationSettings
    , SmpteTtDestinationSettings
    , smpteTtDestinationSettings

    -- * StandardHlsSettings
    , StandardHlsSettings
    , standardHlsSettings
    , shsAudioRenditionSets
    , shsM3u8Settings

    -- * StartTimecode
    , StartTimecode
    , startTimecode
    , sTimecode

    -- * StaticImageActivateScheduleActionSettings
    , StaticImageActivateScheduleActionSettings
    , staticImageActivateScheduleActionSettings
    , siasasImageX
    , siasasHeight
    , siasasFadeOut
    , siasasWidth
    , siasasOpacity
    , siasasLayer
    , siasasDuration
    , siasasImageY
    , siasasFadeIn
    , siasasImage

    -- * StaticImageDeactivateScheduleActionSettings
    , StaticImageDeactivateScheduleActionSettings
    , staticImageDeactivateScheduleActionSettings
    , sidsasFadeOut
    , sidsasLayer

    -- * StaticKeySettings
    , StaticKeySettings
    , staticKeySettings
    , sksKeyProviderServer
    , sksStaticKeyValue

    -- * StopTimecode
    , StopTimecode
    , stopTimecode
    , stLastFrameClippingBehavior
    , stTimecode

    -- * TeletextDestinationSettings
    , TeletextDestinationSettings
    , teletextDestinationSettings

    -- * TeletextSourceSettings
    , TeletextSourceSettings
    , teletextSourceSettings
    , tssPageNumber

    -- * TemporalFilterSettings
    , TemporalFilterSettings
    , temporalFilterSettings
    , tfsStrength
    , tfsPostFilterSharpening

    -- * TimecodeConfig
    , TimecodeConfig
    , timecodeConfig
    , tcSyncThreshold
    , tcSource

    -- * TtmlDestinationSettings
    , TtmlDestinationSettings
    , ttmlDestinationSettings
    , tdsStyleControl

    -- * UdpContainerSettings
    , UdpContainerSettings
    , udpContainerSettings
    , ucsM2tsSettings

    -- * UdpGroupSettings
    , UdpGroupSettings
    , udpGroupSettings
    , ugsTimedMetadataId3Period
    , ugsInputLossAction
    , ugsTimedMetadataId3Frame

    -- * UdpOutputSettings
    , UdpOutputSettings
    , udpOutputSettings
    , uosFecOutputSettings
    , uosBufferMsec
    , uosDestination
    , uosContainerSettings

    -- * VideoCodecSettings
    , VideoCodecSettings
    , videoCodecSettings
    , vcsFrameCaptureSettings
    , vcsH265Settings
    , vcsH264Settings

    -- * VideoDescription
    , VideoDescription
    , videoDescription
    , vdHeight
    , vdSharpness
    , vdWidth
    , vdScalingBehavior
    , vdRespondToAfd
    , vdCodecSettings
    , vdName

    -- * VideoSelector
    , VideoSelector
    , videoSelector
    , vsSelectorSettings
    , vsColorSpaceUsage
    , vsColorSpace

    -- * VideoSelectorPid
    , VideoSelectorPid
    , videoSelectorPid
    , vspPid

    -- * VideoSelectorProgramId
    , VideoSelectorProgramId
    , videoSelectorProgramId
    , vspiProgramId

    -- * VideoSelectorSettings
    , VideoSelectorSettings
    , videoSelectorSettings
    , vssVideoSelectorProgramId
    , vssVideoSelectorPid

    -- * WebvttDestinationSettings
    , WebvttDestinationSettings
    , webvttDestinationSettings
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Product
import Network.AWS.MediaLive.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-10-14@ of the Amazon Elemental MediaLive SDK configuration.
mediaLive :: Service
mediaLive
  = Service{_svcAbbrev = "MediaLive", _svcSigner = v4,
            _svcPrefix = "medialive", _svcVersion = "2017-10-14",
            _svcEndpoint = defaultEndpoint mediaLive,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "MediaLive",
            _svcRetry = retry}
  where retry
          = Exponential{_retryBase = 5.0e-2, _retryGrowth = 2,
                        _retryAttempts = 5, _retryCheck = check}
        check e
          | has (hasCode "ThrottledException" . hasStatus 400)
              e
            = Just "throttled_exception"
          | has (hasStatus 429) e = Just "too_many_requests"
          | has (hasCode "ThrottlingException" . hasStatus 400)
              e
            = Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e =
            Just "throttling"
          | has (hasStatus 504) e = Just "gateway_timeout"
          | has
              (hasCode "RequestThrottledException" . hasStatus 400)
              e
            = Just "request_throttled_exception"
          | has (hasStatus 502) e = Just "bad_gateway"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Placeholder documentation for GatewayTimeoutException
_GatewayTimeoutException :: AsError a => Getting (First ServiceError) a ServiceError
_GatewayTimeoutException
  = _MatchServiceError mediaLive
      "GatewayTimeoutException"
      . hasStatus 504

-- | Placeholder documentation for UnprocessableEntityException
_UnprocessableEntityException :: AsError a => Getting (First ServiceError) a ServiceError
_UnprocessableEntityException
  = _MatchServiceError mediaLive
      "UnprocessableEntityException"
      . hasStatus 422

-- | Placeholder documentation for ConflictException
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException
  = _MatchServiceError mediaLive "ConflictException" .
      hasStatus 409

-- | Placeholder documentation for ForbiddenException
_ForbiddenException :: AsError a => Getting (First ServiceError) a ServiceError
_ForbiddenException
  = _MatchServiceError mediaLive "ForbiddenException" .
      hasStatus 403

-- | Placeholder documentation for NotFoundException
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException
  = _MatchServiceError mediaLive "NotFoundException" .
      hasStatus 404

-- | Placeholder documentation for TooManyRequestsException
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException
  = _MatchServiceError mediaLive
      "TooManyRequestsException"
      . hasStatus 429

-- | Placeholder documentation for InternalServerErrorException
_InternalServerErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerErrorException
  = _MatchServiceError mediaLive
      "InternalServerErrorException"
      . hasStatus 500

-- | Placeholder documentation for BadGatewayException
_BadGatewayException :: AsError a => Getting (First ServiceError) a ServiceError
_BadGatewayException
  = _MatchServiceError mediaLive "BadGatewayException"
      . hasStatus 502

-- | Placeholder documentation for BadRequestException
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException
  = _MatchServiceError mediaLive "BadRequestException"
      . hasStatus 400
