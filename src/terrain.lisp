(in-package #:curseless)

(defun s-expression-to-sentence (sexp)
  (let* ((with-commas (mapcar (lambda (x)
                                (if (eq x 'comma)
                                    ","
                                    (string-downcase (string x))))
                              sexp))
         (joined (apply #'concatenate 'string
                        (loop for (a b) on with-commas
                              collect (if (equal b ",")
                                          a
                                          (concatenate 'string a " ")))))
         (trimmed (string-trim " " joined))
         (capitalized (concatenate 'string
                                   (string-upcase (subseq trimmed 0 1))
                                   (subseq trimmed 1))))
    (concatenate 'string capitalized ".")))

(defun sentences-from-s-expressions (sexps)
  (mapcar #'s-expression-to-sentence sexps))

(defparameter +field-sentences+
  '((the grassy terrain is soft underfoot comma dotted with patches of
     wildflowers)
    (a gentle breeze rustles the tall blades of grass comma creating a
     quiet whisper)
    (small tufts of clover peek through the green expanse comma adding
     texture to the field)
    (dew glistens on the grass comma catching the morning light like
     scattered jewels)
    (the ground beneath the grass is uneven comma with subtle rises
     and dips)
    (a narrow dirt path cuts through the grass comma worn down by
     passing footsteps)
    (tiny insects flit between the blades comma disappearing into the
     green)
    (the scent of fresh grass mixes with the earthy undertone of damp
     soil)
    (soft shadows from scattered clouds dance across the field)
    (a lone dandelion stands upright among the grass comma its yellow
     bloom vibrant against the green)
    (a small cluster of mushrooms grows near the edge of the field
     comma their caps pale in the morning sun)
    (wild strawberries peek from beneath the leaves comma their red
     hue barely visible among the greenery)
    (the field stretches endlessly comma a sea of green under the wide
     sky)
    (a family of rabbits hops across the field comma pausing to nibble
     at the grass)
    (the sun begins to dip behind the distant hills comma casting a
     golden glow over the terrain)
    (a light mist rises from the ground comma blurring the horizon
     with soft tendrils of white)
    (birds soar overhead comma their calls echoing across the open
     space)
    (the field seems alive with the rustling of leaves comma as the
     wind picks up again)
    (a distant creek burbles softly comma its sound blending with the
     whisper of the grass)
    (the vibrant green of the grass contrasts sharply with the deep
     brown of the earth beneath it)
    (wildflowers of all colors bloom in patches comma creating a
     patchwork of hues across the field)
    (an old comma weathered fence lines the edge of the field comma
     its wood faded and splintered by time)
    (a lone tree stands at the far edge of the field comma its
     branches reaching toward the sky)
    (the air feels fresh and cool comma with a hint of moisture from
     the early morning fog)
    (as the sun rises higher comma the dew on the grass begins to
     evaporate in the warmth of the light)
    (the field holds a quiet solitude comma broken only by the sounds
     of nature in the distance)
    (the scent of earth and grass fills the air comma mingling with
     the occasional sweetness of wildflowers)
    (patches of sunlight filter through the leaves of a nearby tree
     comma creating pools of warmth on the ground)
    (the wind rustles the topmost blades of grass comma sending
     ripples across the expanse of green)))

(defun random-field-description ()
  (s-expression-to-sentence (rand-nth +field-sentences+)))

(comment
 (random-field-description)
 ;;=>
 '"The scent of earth and grass fills the air, mingling with the
 occasional sweetness of wildflowers.")


(defparameter +shallow-water-sentences+
  '((the clear water ripples gently comma disturbed by the faintest
     breeze)
    (tiny bubbles rise from the soft mud below comma breaking the
     surface with quiet pops)
    (the water shimmers under the sunlight comma reflecting the sky in
     rippling patterns)
    (a thin layer of algae drifts on the surface comma shifting with
     the slow movement of the water)
    (small insects skitter across the water comma their legs barely
     breaking the surface tension)
    (the scent of damp earth and decaying leaves lingers in the air
     comma mixing with the cool freshness of water)
    (occasional rings spread outward comma marking the spots where
     droplets fall from above)
    (the water is shallow enough to reveal smooth stones beneath comma
     their colors muted by the liquid veil)
    (wisps of submerged plants sway with the current comma their
     tendrils dancing in slow motion)
    (the muddy bottom is uneven comma pockmarked with tiny depressions
     where creatures have moved)
    (sunlight filters through the water comma casting shifting golden
     patterns on the sediment below)
    (the surface of the water is broken by occasional floating debris
     comma leaves and twigs drifting aimlessly)
    (a faint gurgling sound emerges as trapped air escapes from below
     comma forming a fleeting disturbance)
    (thin strands of submerged grass reach upward comma their tips
     barely brushing the surface)
    (the water is tinged with a faint green hue comma clouded by
     drifting particles of silt)
    (a school of tiny fish darts just beneath the surface comma
     scattering at the slightest motion)
    (the shallow water is warm to the touch comma absorbing the heat
     of the sun above)
    (ripples bounce off each other comma forming intricate patterns
     that shift and dissolve)
    (the reflection of the sky is broken by small disturbances comma
     distorting the clouds into wavering shapes)
    (clusters of tiny water striders glide effortlessly comma their
     movements barely perceptible)
    (the surface is glassy and still comma disturbed only by the
     occasional ripple of unseen motion)
    (the mud beneath the water is soft comma shifting slightly with
     each disturbance)
    (thin trails of foam collect in small patches on the water comma
     forming delicate swirling patterns)
    (a single floating petal drifts lazily comma spinning as it
     catches in the tiny currents)
    (water beetles dart just below the surface comma their dark shapes
     flashing as they turn)
    (a faint earthy scent rises from the water comma mingling with a
     hint of something metallic)
    (the water appears shallow yet bottomless comma its murky depths
     hiding the smallest movements)
    (each step in the mud below sends slow waves outward comma marking
     the disturbance)
    (the surface is dappled with sunlight comma broken by shadows of
     passing clouds)))

(defun random-water-description ()
  (s-expression-to-sentence (rand-nth +shallow-water-sentences+)))

(defparameter +forest-sentences+
  '((the air is thick with the scent of damp earth comma bark comma and fallen leaves)
    (shafts of sunlight pierce through the canopy comma casting
     shifting patches of light on the forest floor)
    (the ground is soft underfoot comma layered with decaying leaves
     and moss)
    (a steady chorus of birdsong echoes through the trees comma
     punctuated by the rustling of unseen creatures)
    (gnarled roots twist along the ground comma forming natural
     obstacles beneath the foliage)
    (the trunks of ancient trees rise high comma their rough bark
     scarred by time)
    (a light mist clings to the undergrowth comma swirling gently as
     the air shifts)
    (ferns and low shrubs crowd the forest floor comma their fronds
     brushing against passing footsteps)
    (the occasional creak of bending wood sounds overhead comma as the
     trees sway in the wind)
    (mushrooms sprout from fallen logs comma their pale caps
     glistening with moisture)
    (a faint trickle of water can be heard comma hidden somewhere
     among the trees)
    (the rich scent of pine needles mingles with the musk of damp
     soil)
    (patches of moss climb the bases of trees comma their green hues
     vibrant in the dim light)
    (clusters of wild fungi grow in the shadows comma their forms
     varied and strange)
    (the undergrowth is dense in places comma a tangle of branches
     comma vines comma and brambles)
    (a squirrel darts up the trunk of a tree comma its claws
     scrabbling against the bark)
    (occasional gusts of wind send dry leaves skittering across the
     ground)
    (the stillness is broken only by the distant tapping of a
     woodpecker)
    (twisted branches form intricate shapes comma their silhouettes
     stark against the dappled sky)
    (fireflies flicker in the gloom comma their tiny lights appearing
     and vanishing in the twilight)
    (the scent of rain lingers comma clinging to the bark and leaves
     with an earthy freshness)
    (a fallen tree lies across the ground comma its wood softened by
     years of decay)
    (the crunch of twigs and leaves underfoot is the only sound in the
     still air)
    (fungal spores drift unseen comma released from the damp recesses
     of rotting logs)
    (the forest is alive with tiny movements comma a world hidden
     beneath the leaves)
    (old branches hang heavy with moss comma draping down like
     tattered curtains)
    (glimpses of movement vanish into the undergrowth comma leaving
     only a rustling trail)
    (dew clings to the edges of leaves comma catching the dim light in
     tiny droplets)
    (the trees stretch high comma their branches interwoven into a
     thick ceiling of green)))

(defun random-forest-description ()
  (s-expression-to-sentence (rand-nth +forest-sentences+)))
