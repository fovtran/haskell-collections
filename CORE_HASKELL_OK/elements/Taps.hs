�j}\�A���
�bDCr#��Õ��8��;?zQ��y���J5(��z9����<Tʕ�e(X=a�MΈ7��g-������.zl�,S\�$���j&��f68�'��bp}q 0���p����֭�n�8�5*���saN}I~ل&" ��������8Ԭo��I�������Gf �yS���&�S���QЮ�yd���e�1nh+a���#s�^��;����UW��!Q���}+�>y��Dȭ��Q�⑙4��p�^q��e	4;y`�o��*��WP�RU���v�_^qHeXR44M!�><h�E�8���[�S�l� ��@��.<�A<7?��s�5���Kh�G/}��U�������򒯦<ȯG�_��ڑT�I����3�w�8t��7y���ɼXȤ0_<?6$"n!B�1�!ƭ]��TD��&$]�"F�LV���ً����L�>JWB^&�&e(�;]v|��6��o��@��G(w���m��M��p0��} ��f�[5|k��k�/ݐ*���N�w�k�7�.�;(�!���,h����B8@�W+��b�1#f����C��H���p0z�H�^g!#�V�YP^��eG6��������u8�)����"�@eb�O���i.�O�*$�	m����~w�w�+ė�� 	���8j��Cs��`��"4ԑ5֗�8bX�-8�^\oo`DP��F��r����z�>m��� ��\��ڲ�/��W\+��G�AzΏ=m�wQhNn0���R8'fjŠ�@0ձ�J\�
�
ɢ@RB���_.��0]B EGFИ%��]���K��%o֊�
�xS�,
��Ѹ�����YUf���(�	�(���(W��QE�
���OP-֑qk���"��Pԗ������]p-y2�������г��ho����Յo;����Fd��RE����Wm��t�� �B	���P8�|�Z7ι�����^���Q�'� 5���\��E��.�$�9`�`4��|f��1���ѽ`�̱Nn�l�G ��6D�E�gu ��kKsPIT(�s�r�����Z�Ć6j�t��	�8i�`@?�4��                                                                                                                                                                                                                                                                                                                                                                                                  -a)) / (pi * (n-a)) - lpf_tap wc m n
    where a = (fromIntegral m) / 2

-- Multiband tap function

mbf_tap :: (Integral a, Floating b, Eq b) => [b] -> [b] -> a -> b -> b
mbf_tap (g:[])     (w:[]) m n = g * lpf_tap w m n
mbf_tap (g1:g2:gs) (w:ws) m n = (g1-g2) * lpf_tap w m n + mbf_tap (g2:gs) ws m n
mbf_tap _          _      _ _ = error "mbf_tap: bands out of sync"

-- Raised-cosine tap function.  This does _not_ have 0 dB DC gain.

-- ws = symbol rate in normalized radians
-- b = filter beta

rc_tap :: (Integral a, Floating b, Eq b) => b -> b -> a -> b -> b
rc_tap ws b m n | n-a == 0  = 1
                | den == 0  = 0
                | otherwise = sin sarg / sarg * cos carg / den
    where sarg = ws * (n-a) / 2
          carg = b * ws * (n-a) / 2
          den = 1 - 4 * ((b*ws*(n-a)) / (2*pi)) ^ (2::Int)
          a = (fromIntegral m) / 2

-- The following functions generate a list of the taps for a given set of
-- parameter.

-- | Lowpass filter

lpf :: (Ix a, Integral a, Enum b, Floating b, Eq b) => b -- ^ wc
       -> a -- ^ M
       -> Array a b -- ^ h[n]

lpf wc m = listArray (0,m) $ map (lpf_tap wc m) (indexes m)

-- | Highpass filter

hpf :: (Ix a, Integral a, Enum b, Floating b, Eq b) => b -- ^ wc
       -> a -- ^ M
       -> Array a b -- ^ h[n]

hpf wc m = listArray (0,m) $ map (hpf_tap wc m) (indexes m)

-- | Bandpass filter

bpf :: (Ix a, Integral a, Enum b, Floating b, Eq b) => b -- ^ wl
       -> b -- ^ wu
       -> a -- ^ M
       -> Array a b -- ^ h[n]

bpf wl wu m = listArray (0,m) $ zipWith (+) (elems $ lpf wu m) (elems $ hpf wl m)

-- | Bandstop filter

bsf :: (Ix a, Integral a, Enum b, Floating b, Eq b) => b -- ^ wl
       -> b -- ^ wu
       -> a -- ^ M
       -> Array a b -- ^ h[n]

bsf wl wu m = listArray (0,m) $ zipWith (+) (elems $ lpf wl m) (elems $ hpf wu m)

-- | Multiband filter

mbf :: (Ix a, Integral a, Enum b, Floating b, Eq b) => [b] -- ^ [mags]
       -> [b] -- ^ [w]
       -> a -- ^ M
       -> Array a b -- ^ h[n]

mbf g w m = listArray (0,m) $ map (mbf_tap g w m) (indexes m)

-- | Raised-cosine filter

rc :: (Ix a, Integral a, Enum b, Floating b, Eq b) => b -- ^ ws
       -> b -- ^ beta
       -> a -- ^ M
       -> Array a b -- ^ h[n]

rc ws b m = listArray (0,m) $ map (rc_tap ws b m) (indexes m)
