window.onload = function() {

    updateVisitCount(); //Update du nombre de visiteurs et init de l'ID du participant en cours.


    //Renvoie une liste suivant le groupe indiqué
    function setList(groupe) {
        let list = [];

        let bleu = ['BLEU_yellow', 'BLEU_blue'];
        let jaune = ['JAUNE_blue', 'JAUNE_yellow'];
        let rouge = ['ROUGE_green', 'ROUGE_red'];
        let vert = ['VERT_red', 'VERT_green'];

        let stumuliParCouleur = nbStimulusParParticipants / 4;

        let nbCongruentB = groupe == 2 ? stumuliParCouleur * 0.2 : stumuliParCouleur * 0.8;
        let nbCongruentJ = nbCongruentB;
        let nbCongruentR = groupe == 2 ? stumuliParCouleur * 0.8 : stumuliParCouleur * 0.2;
        let nbCongruentV = nbCongruentR;

        let idx_congru_B = idx_congru_J = idx_congru_R = idx_congru_V = 0;
        let nb_B = nb_J = nb_R = nb_V = 0;

        while (nb_B < stumuliParCouleur || nb_J < stumuliParCouleur || nb_R < stumuliParCouleur || nb_V < stumuliParCouleur) {
            if (idx_congru_B < nbCongruentB) {
                list.push(bleu[0]);
                idx_congru_B += 1;
                nb_B += 1;
            } else if (idx_congru_J < nbCongruentJ) {
                list.push(jaune[0])
                idx_congru_J += 1;
                nb_J += 1;
            } else if (idx_congru_R < nbCongruentR) {
                list.push(rouge[0])
                idx_congru_R += 1;
                nb_R += 1;
            } else if (idx_congru_V < nbCongruentV) {
                list.push(vert[0]);
                idx_congru_V++;
                nb_V += 1;
            } else {
                if (nb_B < stumuliParCouleur) {
                    list.push(bleu[1]);
                    nb_B += 1;
                } else if (nb_J < stumuliParCouleur) {
                    list.push(jaune[1]);
                    nb_J += 1;
                } else if (nb_R < stumuliParCouleur) {
                    list.push(rouge[1]);
                    nb_R += 1;
                } else {
                    list.push(vert[1]);
                    nb_V += 1;
                }
            }
        }
        return list;
    };

    function getList() {
        if (!init) {
            let list;
            init = true;
            list = shuffle(setList(groupe));
            return list;
        }
    }

    function check_shuffle(array) {
        var temp;

        for (let i = 1; i < array.length - 1; i++) {
            if (array[i - 1].split("_")[1] == array[i].split("_")[1] || array[i + 1].split("_")[1] == array[i].split("_")[1]) {
                for (let j = i; j < array.length; j++) {

                    if (array[j - 1].split("_")[1] != array[i].split("_")[1] &&
                        array[j + 1].split("_")[1] != array[i].split("_")[1] &&
                        array[j].split("_")[1] != array[i].split("_")[1] &&
                        array[i - 1].split("_")[1] != array[j].split("_")[1] &&
                        array[i + 1].split("_")[1] != array[j].split("_")[1]) {

                        temp = array[j];
                        array[j] = array[i];
                        array[i] = temp;

                        break;
                    }
                }
            }
        }
        return array;
    }

    function shuffle(array) {
        var i = array.length,
            j = 0,
            temp;

        while (i--) {
            if (i != 0 && i != array.length - 1) {
                j = Math.floor(Math.random() * (i + 1));
                let essai = 0;
                while (array[i - 1].split("_")[1] == array[j].split("_")[1] || array[i + 1].split("_")[1] == array[j].split("_")[1]) {
                    if (essai > 15) {
                        break;
                    }
                    j = Math.floor(Math.random() * (i + 1));
                    essai += 1;
                }
            }
            temp = array[i];
            array[i] = array[j];
            array[j] = temp;
        }
        return check_shuffle(array);
    }

    function getStimuliEntrainement() {
        var lastColorIndex = sessionStorage.getItem('lastci');
        var lastTextIndex = sessionStorage.getItem('lastti');

        if (lastColorIndex == "" || lastColorIndex == null) {
            sessionStorage.setItem('lastci', -1);
            lastColorIndex = sessionStorage.getItem('lastci');
        }

        if (lastTextIndex == "" || lastTextIndex == null) {
            sessionStorage.setItem('lastti', -1);
            lastTextIndex = sessionStorage.getItem('lastti');
        }

        //Do not repeat last color.
        var colorIndex = Math.floor(couleurs.length * Math.random());
        while (colorIndex == lastColorIndex) {
            colorIndex = Math.floor(couleurs.length * Math.random());
        }

        //Do not repeat last text.
        var textIndex = Math.floor(mots.length * Math.random());
        while (textIndex == lastTextIndex) {
            textIndex = Math.floor(mots.length * Math.random());
        }

        sessionStorage.setItem('lastci', colorIndex);
        sessionStorage.setItem('lastti', textIndex);

        var mot = mots[textIndex];
        var couleur = couleurs[colorIndex];

        return [mot, couleur];
    }

    function updateVisitCount() {
        var current = Boolean(sessionStorage.getItem('session')); //boolean pour savoir si l'utilisateur refresh sa page
        if (!current) {
            fetch('https://api.countapi.xyz/update/experience/participant/?amount=1')
                .then(res => res.json())
                .then(res => {
                    sessionStorage.setItem('session', true);
                    sessionStorage.setItem('id', res.value);
                    id_participant = res.value;
                })
        }
    }

    //Constantes
    const mots = ['BLEU', 'ROUGE', 'JAUNE', 'VERT'];
    const couleurs = ['blue', 'red', 'yellow', 'green'];
    const trad = { 'blue': 'BLEU', 'red': 'ROUGE', 'yellow': 'JAUNE', 'green': 'VERT' };
    const nbStimulusParParticipants = 120;
    const maxNumberOfTrials = 120; //Max essai phase d'experience. Doit etre à 120
    const maxNumberOfTrials_t = 16; //Max essai phase d'entrainement. Doit etre à 16
    const inter_trial_interval = 500; //Temps entre chaque essais
    const refreshing = 16; //Interval entre chaque capture de la mouse position

    //Variables
    var currentNumberOfTrials = 0;
    var currentNumberOfTrials_t = 0;
    var essais = new Array();
    var init = false;
    var stimuli;
    var mot;
    var couleur;
    var passage_en_cours = false;
    var entrainement_fini = false;
    var temp_x = 0;
    var temp_y = 0;
    var groupe; //Groupe affecté selon le nombre de participants déjà passés
    var liste;
    var id_participant;

    //Stockage
    var reaction_time;
    var mot_s;
    var couleur_s;
    var curve_x = new Array();
    var curve_y = new Array();
    var congruent;
    var tmp_init;
    var error;
    var tmp_start;
    var tmp_end;
    var tmp_move;


    //Lorsqu'on clique sur le bouton "Commencer", on remplace la div d'introduction par la div de l'expérience
    document.getElementById("bouton-commencer").addEventListener("click", function() {
        document.getElementById("cadre-intro").style.display = "none";
        document.getElementById("cadre-experience").style.display = "block";
    });

    //Lorsqu'on clique sur le bouton "Start", on lance le stimulus, le timer et le mouse tracker
    document.getElementById("bouton-start").addEventListener("click", function() {
        document.getElementById("bouton-start").style.display = "none";

        //Gestion de l'affichage du compteur d'essais
        if (!entrainement_fini) {
            document.getElementById("affichage-compteur").innerHTML = currentNumberOfTrials_t + 1 + " / " + maxNumberOfTrials_t;
        } else {
            document.getElementById("affichage-compteur").innerHTML = currentNumberOfTrials + 1 + " / " + maxNumberOfTrials;
        }

        //Initialisation
        stimuli = entrainement_fini ? liste[currentNumberOfTrials].split("_") : getStimuliEntrainement();
        mot = stimuli[0];
        couleur = stimuli[1];
        tmp_move = 0;
        passage_en_cours = true;

        curve_x = new Array();
        curve_y = new Array();

        //Affichage du mot et de sa couleur
        setTimeout(() => {
            tmp_start = Date.now();
            document.getElementById("affichage-stimulus").innerHTML = mot;
        }, 300);
        document.getElementById("affichage-stimulus").style.color = couleur;

        //Sauvegarde des état de congruence, mot et couleur affichés
        congruent = stimuli[0] == trad[stimuli[1]] ? 1 : 0;
        mot_s = mot;
        couleur_s = couleur;
    });

    //Pour chaque div resultat, lorsqu'on clique, on enregistre les résultats et re-initialise
    var listeDesReponses = document.getElementsByClassName("resultat");
    for (var i = 0; i < listeDesReponses.length; i++) {
        listeDesReponses[i].addEventListener("click", function() {


            //Arrêt du timer
            passage_en_cours = false;
            temp_x = 0;
            temp_y = 0;
            tmp_end = Date.now();
            reaction_time = (tmp_end - tmp_start) / 1000;

            //Vérification de la réponse
            error = this.innerHTML != trad[couleur] ? 1 : 0;

            //Si erreur, affichage d'un 'X' rouge pendant 2s puis réinitialisation
            if (error) {
                document.getElementById("affichage-stimulus").innerHTML = "X";
                document.getElementById("affichage-stimulus").style.color = "red";

                setTimeout(() => {
                    document.getElementById("affichage-stimulus").innerHTML = "";
                    document.getElementById("bouton-start").style.display = "block";
                }, 2000);

            } else if (tmp_init < 0) {
                document.getElementById("affichage-stimulus").style.fontSize = "medium";
                document.getElementById("affichage-stimulus").innerHTML = "Ne pas déplacer la souris AVANT l'apparation du stimulus";
                document.getElementById("affichage-stimulus").style.color = "red";

                setTimeout(() => {
                    document.getElementById("affichage-stimulus").style.fontSize = "xxx-large";
                    document.getElementById("affichage-stimulus").innerHTML = "";
                    document.getElementById("bouton-start").style.display = "block";
                }, 2000);
            } else { //Sinon, réinitialisation
                document.getElementById("affichage-stimulus").innerHTML = "";
                setTimeout(() => { document.getElementById("bouton-start").style.display = "block"; }, inter_trial_interval);
            }

            //Enregistrement des données de l'essais
            if (entrainement_fini) {
                essais.push({
                    congruent: congruent,
                    mot: mot_s,
                    couleur: couleur_s,
                    reactionTime: reaction_time,
                    tempsInit: tmp_init,
                    courbeX: curve_x,
                    courbeY: curve_y,
                    erreur: error
                });
            }

            curve_x = new Array();
            curve_y = new Array();

            if (entrainement_fini) {
                currentNumberOfTrials += 1;
            } else {
                currentNumberOfTrials_t += 1;
            }

            //Si le nombre d'essais est au maximum, on termine la passation
            if (currentNumberOfTrials == maxNumberOfTrials) {

                document.getElementById("cadre-experience").style.display = "none";
                document.getElementById("fin").style.display = "block";

                var resultats = new Array();
                resultats.push({
                    id: id_participant,
                    groupe: groupe,
                    essais: essais
                });

                savedata(resultats);
                //console.log(resultats);

            } else if (currentNumberOfTrials_t == maxNumberOfTrials_t && !entrainement_fini) { //Si on a fini la phase d'entrainement
                document.getElementById("cadre-intro").style.display = "block";
                document.getElementById("cadre-experience").style.display = "none";
                document.getElementById("consigne").style.display = "none";
                document.getElementById("consigne-fin-entrainement").style.display = "block";
                entrainement_fini = true;

                //Initisalisation des variables utiles
                liste = getList();
                if (!id_participant) {
                    id_participant = Number(sessionStorage.getItem('id'));
                }
                groupe = 2;
                console.log("Liste : " + liste);
                console.log("ID :" + id_participant);
                console.log("Groupe :" + groupe);
            }
        });
    };

    //Fonction mouse tracking
    (function() {
        var mousePos;
        document.onmousemove = handleMouseMove;
        //setInterval repeats every X ms
        setInterval(getMousePosition, refreshing);

        function handleMouseMove(event) {

            var dot, eventDoc, doc, body, pageX, pageY;

            event = event || window.event;

            if (event.pageX == null && event.clientX != null) {
                eventDoc = (event.target && event.target.ownerDocument) || document;
                doc = eventDoc.documentElement;
                body = eventDoc.body;

                event.pageX = event.clientX +
                    (doc && doc.scrollLeft || body && body.scrollLeft || 0) -
                    (doc && doc.clientLeft || body && body.clientLeft || 0);
                event.pageY = event.clientY +
                    (doc && doc.scrollTop || body && body.scrollTop || 0) -
                    (doc && doc.clientTop || body && body.clientTop || 0);
            }
            mousePos = {
                x: event.pageX,
                y: event.pageY
            };
        }

        function getMousePosition() {
            if (mousePos) {
                //console.log("x = " + mousePos.x+ "; y = " +mousePos.y);
                if (currentNumberOfTrials != maxNumberOfTrials) {
                    if ((temp_x == 0 || temp_y == 0) && passage_en_cours) { //Position de la souris lors du clique du bouton start
                        temp_x = mousePos.x;
                        temp_y = mousePos.y;
                    }
                    if ((temp_x != mousePos.x || temp_y != mousePos.y) && passage_en_cours) { //Premier mouvement après clique bouton start
                        tmp_move = (tmp_move == null || tmp_move == 0) ? Date.now() : tmp_move;
                        tmp_init = (tmp_move - tmp_start) / 1000;
                    }
                    curve_x.push(mousePos.x);
                    curve_y.push(mousePos.y);
                }
            }
        }
    })();
};