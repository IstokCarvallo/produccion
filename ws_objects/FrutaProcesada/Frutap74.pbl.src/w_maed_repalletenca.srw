$PBExportHeader$w_maed_repalletenca.srw
forward
global type w_maed_repalletenca from w_mant_encab_deta_csd
end type
type dw_3 from uo_dw within w_maed_repalletenca
end type
type dw_4 from datawindow within w_maed_repalletenca
end type
type dw_5 from datawindow within w_maed_repalletenca
end type
end forward

global type w_maed_repalletenca from w_mant_encab_deta_csd
integer width = 3593
integer height = 2064
string title = "REPALLETIZADO"
string menuname = ""
event ue_imprimir ( )
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
end type
global w_maed_repalletenca w_maed_repalletenca

type variables
w_mant_deta_repalletdeta_salida iw_mantencion

DataWindowChild	dw_planta

Integer	ii_dw, il_fila2, ii_tiporepa
Boolean	lb_escambiodealtura, lb_actualizatablas, lb_eliminapallet = False, lb_borrapallet = False

Str_mant		istr_mant2

DataStore ids_Palletfruta_pafr_fecing	
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public function boolean noexistecliente (integer ai_codigo)
public subroutine habilitaingreso (string columna)
public subroutine listaproductores ()
public function boolean existefolio (string as_columna, string as_valor)
public subroutine cuentatarjas ()
public subroutine generalistavariedad ()
public function string buscacliente (integer cliente)
public function string buscaplanta (integer planta)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine separaportipood ()
public function boolean borrahistoria (integer cliente, integer planta, long numero)
public function long buscanuevofolio (integer planta)
public function boolean eliminapallet (long pallet)
public function boolean actuaestadopallet ()
public function boolean borrapallet ()
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Integer	li_cliente, li_planta
String	ls_Fecha, ls_numero, ls_cliente, ls_planta
str_info	lstr_info

IF lb_escambiodealtura THEN
	lstr_info.titulo	= "CAMBIOS DE ALTURA/PRODUCTOR/CALIBRE"
ELSE
	lstr_info.titulo	= "RECEPCION DE PALLETS"
END IF
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

ls_fecha		=	String(dw_2.Object.repe_fecrep[1])
ls_numero	=	String(dw_2.Object.repe_numero[1],'00000000')

li_cliente		=	dw_2.Object.clie_codigo[1]
li_planta		=	dw_2.Object.plde_codigo[1]

ls_cliente		= 	String(li_cliente,'###')	+" "+	BuscaCliente(li_cliente)
ls_planta		= 	String(li_planta,'####')	+" "+	BuscaPlanta(li_Planta)

IF lb_escambiodealtura THEN
	vinf.dw_1.DataObject = "dw_info_cambioaltura"
	vinf.dw_1.SetTransObject(sqlca)

	fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]), &	
									Long(istr_mant.argumento[2]), &
									Integer(istr_mant.argumento[3]))
ELSE
	vinf.dw_1.DataObject = "dw_info_repaletizado"
	vinf.dw_1.SetTransObject(sqlca)

	fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]), &	
									Long(istr_mant.argumento[2]), &
									Integer(istr_mant.argumento[3]),-1)
END IF

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)

	IF lb_escambiodealtura THEN
		vinf.dw_1.Modify("Fecha.text = '" + ls_fecha + "'")
		vinf.dw_1.Modify("nroguia.text = '" + ls_numero + "'")
		vinf.dw_1.Modify("cliente.text = '" + ls_cliente + "'")
		vinf.dw_1.Modify("planta.text = '" + ls_planta + "'")
	END IF

	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.SetTabOrder("clie_codigo",10)
	dw_2.SetTabOrder("plde_codigo",20)
	dw_2.SetTabOrder("repe_numero",30)
	dw_2.SetTabOrder("repe_fecrep",40)
	dw_2.SetTabOrder("repe_cantar",50)
	dw_2.SetTabOrder("repe_tardef",60)
	dw_2.SetTabOrder("repe_tipopa",70)
	dw_2.Modify("repe_numero.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(rgb(255,255,255)))	
	dw_2.Modify("repe_tipopa.BackGround.Color = " + String(rgb(255,255,255)))	
	dw_2.SetColumn("repe_numero")
	dw_2.SetFocus()
ELSE
	dw_2.SetTabOrder("repe_numero",0)
	dw_2.SetTabOrder("plde_codigo",0)
	dw_2.SetTabOrder("clie_codigo",0)	
	dw_2.SetTabOrder("repe_tipopa",0)
	dw_2.Modify("repe_numero.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("repe_tipopa.BackGround.Color = " + String(RGB(166,180,210)))
END IF
end subroutine

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre  
   FROM	dba.clientesprod
   WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN True
END IF

IF F_ValidaCliente(ai_codigo) THEN
	istr_mant.Argumento[3]	=	String(ai_codigo)
	RETURN False
ELSE
	RETURN True
END IF


end function

public subroutine habilitaingreso (string columna);Date	ld_fecha
Integer  li_tarjas, li_tardef
Boolean	lb_estado = True
String 	ls_usuario

dw_2.AcceptText()

li_tarjas = dw_2.Object.repe_cantar[1]
li_tardef =	dw_2.Object.repe_tardef[1]
ls_usuario=	dw_2.Object.repe_usuari[1]

IF IsNull(li_tarjas) THEN li_tarjas = 0
IF IsNull(li_tardef) THEN li_tardef = 0
IF IsNull(ls_usuario) THEN ls_usuario = ''

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.repe_fecrep[1]) OR dw_2.Object.repe_fecrep[1] = ld_fecha OR &
		IsNull(dw_2.Object.repe_usuari[1]) OR dw_2.Object.repe_usuari[1] = '' OR &
		li_tarjas + li_tardef = 0 THEN
		lb_estado = False
	END IF
END IF
dw_3.Enabled = Not lb_escambiodealtura
pb_ins_det.Enabled = lb_estado


end subroutine

public subroutine listaproductores ();Integer	li_Cliente, li_Planta
Long		ll_Fila, ll_NroPallet, ll_Productor,ll_Fila2
String		ls_Productores
Date		ld_pafr_fecing, ld_pafr_fecemb

istr_mant.argumento[8]		=	""
istr_mant.argumento[32]	=	""
istr_mant.argumento[33]	=	""

FOR ll_Fila = 1 TO dw_3.RowCount()
	li_Cliente		=	dw_3.Object.clie_codigo[ll_Fila]
	ll_NroPallet	=	dw_3.Object.paen_numero[ll_Fila]
	li_Planta		=	dw_3.Object.plde_codigo[ll_Fila]
	
	SELECT	LIST(DISTINCT prod_codigo)
		INTO	:ls_Productores
		FROM	dba.palletfruta
		WHERE	clie_codigo	=	:li_Cliente
		AND	paen_numero	=	:ll_NroPallet
		AND	plde_codigo	=	:li_Planta ;

	IF Not Isnull( ls_Productores) AND ls_Productores <> "" THEN
		IF Pos(istr_mant.argumento[8], "," + ls_Productores) = 0 THEN
			istr_mant.argumento[8] +=	"," + ls_Productores
		END IF
	END IF
	
	SELECT	LIST(DISTINCT pafr_fecing)
		INTO	:ld_pafr_fecing
		FROM	dba.palletfruta
		WHERE	clie_codigo	=	:li_Cliente
		AND	paen_numero	=	:ll_NroPallet
		AND	plde_codigo	=	:li_Planta ;
	
	IF Not Isnull( ld_pafr_fecing) AND ld_pafr_fecing <> Date('1900-01-01') THEN
		IF Pos(istr_mant.argumento[32], "," + String(ld_pafr_fecing)) = 0 THEN
			istr_mant.argumento[32] +=","+String(ld_pafr_fecing)		
		END IF
	END IF

	SELECT	LIST(DISTINCT pafr_fecemb)
		INTO	:ld_pafr_fecemb
		FROM	dba.palletfruta
		WHERE	clie_codigo	=	:li_Cliente
		AND	paen_numero	=	:ll_NroPallet
		AND	plde_codigo	=	:li_Planta ;
		
	IF Not Isnull( ld_pafr_fecemb) AND ld_pafr_fecemb <> Date('1900-01-01') THEN
		IF Pos(istr_mant.argumento[33], "," + String(ld_pafr_fecemb)) = 0 THEN
			istr_mant.argumento[33] +=","+String(ld_pafr_fecemb)		
		END IF
	END IF
	
NEXT
	
RETURN
end subroutine

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_tipopa, li_cliente
Long		ll_nfolio

li_cliente  =	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.repe_numero[1]

CHOOSE CASE as_columna
	CASE "clie_codigo"
		li_cliente	=	Integer(as_valor)
		
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "repe_numero"
		ll_nfolio 	=	Long(as_valor)
		
END CHOOSE

SELECT	repe_tipopa,Count(*) 
	INTO	 :li_tipopa, :li_existe
	FROM	dba.REPALLETENCA
	WHERE	clie_codigo =  :li_cliente
	AND   plde_codigo	=	:li_planta
	AND	repe_numero	=	:ll_nfolio
	GROUP BY repe_tipopa;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Repalletenca")
	RETURN True
ELSE
	IF li_tipopa = 6 THEN
		Messagebox("Advertencia","Número de Repalletizado corresponde a Cambio de Folio")
		RETURN True
	ELSE
			IF li_existe > 0 THEN
				istr_mant.argumento[1]	= String(li_planta)
				istr_mant.argumento[2]	= String(ll_nfolio)
				istr_mant.argumento[3]	= String(li_cliente)

				This.TriggerEvent("ue_recuperadatos")
			
				istr_mant.argumento[4]	= String(dw_2.Object.repe_cantar[1])
				RETURN False
			ELSE
				MessageBox("Atención","Número de Documento No ha sido generado. Ingrese Otro.")
 			 	RETURN True
//				istr_mant.argumento[1]	= String(li_planta)
//				istr_mant.argumento[2]	= String(ll_nfolio)
//				RETURN False
			END IF
		END IF
	  RETURN False
END IF

end function

public subroutine cuentatarjas ();Long I,ll_tra=0,ll_def=0, ll_cantar, ll_tardef

FOR I=1 TO dw_1.Rowcount()
	IF dw_1.Object.paen_tipopa[I]=1 THEN
		ll_def ++
	ELSE
		ll_tra ++
	END IF
NEXT

ll_cantar	=	dw_2.Object.repe_cantar[1]
ll_tardef	=	dw_2.Object.repe_tardef[1]

istr_mant.argumento[10]	= String(ll_cantar + ll_tardef)
istr_mant.argumento[12]	= String(dw_2.Object.repe_cantar[1] )
istr_mant.argumento[11]	= String(dw_2.Object.repe_tardef[1] )
		
RETURN
end subroutine

public subroutine generalistavariedad ();Long		ll_Fila, ll_NroPallet
String	ls_variedades
Date     ld_fechaembal, ld_fechaminima, ld_fecha
Integer  li_Cliente,li_Planta

istr_mant.argumento[22]	=	""
istr_mant.argumento[23]	=	""

FOR ll_Fila = 1 TO dw_3.RowCount()
	li_Cliente		=	dw_3.Object.clie_codigo[ll_Fila]
	ll_NroPallet	=	dw_3.Object.paen_numero[ll_Fila]
	li_Planta		=	dw_3.Object.plde_codigo[ll_Fila]
   ld_fechaembal	=  dw_3.Object.paen_fecemb[ll_fila]
	
	IF ll_fila<>1 THEN
		IF ld_fechaembal < ld_fechaminima AND ld_fechaembal <> ld_fecha THEN
			ld_fechaminima = ld_fechaembal
		END IF	
	ELSE
		ld_fechaminima = ld_fechaembal
	END IF 

	SELECT	LIST(DISTINCT vari_codigo)
		INTO	:ls_variedades
		FROM	dba.palletfruta
		WHERE	clie_codigo	=	:li_Cliente
		AND	paen_numero	=	:ll_NroPallet
		AND	plde_codigo	=	:li_Planta ;
	
	
		istr_mant.argumento[22] +=	"," + ls_variedades
	
NEXT
	
IF ld_fechaminima <> ld_fecha THEN istr_mant.argumento[23] = String(ld_fechaminima,'dd/mm/yyyy')
	
RETURN



end subroutine

public function string buscacliente (integer cliente);String	ls_Cliente

SELECT clie_nombre
INTO :ls_Cliente
FROM dba.clientesprod
WHERE clie_codigo = :cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN "No Existe Cliente"
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN "No Existe Cliente"
	
END IF


RETURN ls_cliente
end function

public function string buscaplanta (integer planta);String	ls_planta

SELECT	plde_nombre
	INTO	:ls_planta 
   FROM	dba.plantadesp  
   WHERE plde_codigo	=	:planta
	AND	plde_tipopl	=	1 ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla PLANTADESP")
	RETURN "No Existe Planta"
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Codigo Planta no Existe. Ingrese otro")
	RETURN "No Existe Planta"
END IF

RETURN ls_planta
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
Long Numero
Integer li_integer, li_movto, li_planta, li_tiporepa

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF dw_5.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					ELSE
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						dw_4.ResetUpdate()
						dw_5.ResetUpdate()
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
			
					RollBack;
				END IF	
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
			
				RollBack;
			END IF	
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF dw_5.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					ELSE
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						dw_4.ResetUpdate()
						dw_5.ResetUpdate()
						pb_nuevo.Enabled = TRUE

					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
			
					RollBack;
				END IF	
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
			
				RollBack;
			END IF	
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit
IF lb_actualizatablas = True THEN
	IF dw_2.Object.repe_tipopa[1] = 1 or dw_2.Object.repe_tipopa[1] = 2 THEN
		IF lb_Retorno = True Then
			borrahistoria(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[47]))
		END IF
	END IF	
END IF

Numero = Long(istr_mant.argumento[2])
li_planta = Integer(istr_mant.argumento[1])
li_movto = 4

/*actualiza numero actual en correlativos */
update DBA.CORRELMOVIMIENTOS set
como_actual = :numero
where plde_codigo = :li_planta
And	como_tipomv = :li_movto;

IF lb_escambiodealtura = False THEN
	IF lb_eliminapallet = True THEN
		actuaestadopallet()
		lb_eliminapallet = False
	END IF
	IF lb_borrapallet = True  THEN
		borrapallet()
		lb_borrapallet = False
	END IF	
END IF	


RETURN lb_Retorno
end function

public subroutine separaportipood ();String 	ls_filtro
Integer	li_i

//ls_filtro = "repd_tipood = 1"
//dw_1.SetFilter(ls_filtro)
//dw_1.Filter()
//
//dw_1.RowsCopy(1, dw_1.RowCount(), Primary!, dw_3, 1, Primary!)
//
//FOR li_i = 1 TO dw_1.RowCount()
//	dw_1.DeleteRow(li_i)
//NEXT
//
//ls_filtro = ""
//dw_1.SetFilter(ls_filtro)
//dw_1.Filter()
end subroutine

public function boolean borrahistoria (integer cliente, integer planta, long numero);Long		ll_PROCESO, ll_PROCESO2, ll_cajas, ll_cajas1
Integer	li_especie, li_variedad, li_productor, li_etiqueta, li_condicion
String	ls_embalaje, ls_calibre

// SELECCIONA EL ULTIMO numero PROCESO REALIZADO EN palletencabhisto
SELECT MAX(PAHI_PROCES)
INTO  :ll_PROCESO
FROM dba.palletencabhisto
WHERE clie_codigo = :cliente
AND   plde_codigo = :planta
AND   paen_numero = :numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla palletencabhisto")
	RETURN False
END IF

// SELECCIONA EL ULTIMO numero PROCESO REALIZADO EN palletfrutahisto
SELECT MAX(PAFH_PROCES)
INTO  :ll_PROCESO2
FROM dba.palletfrutahisto
WHERE clie_codigo = :cliente
AND   plde_codigo = :planta
AND   paen_numero = :numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla palletfrutahisto")
	RETURN False
END IF

/*Actualiza las cajas al estado anterior en las tablas PALLETENCAB, PALLETFRUTA siempre que sea tipo 
pallet 1 o 2 rebaje o levantamiento de altura*/
UPDATE dba.PALLETENCAB as pal
SET	pal.PAEN_CCAJAS = pae.PAEN_CCAJAS,
		pal.tpem_codigo = pae.tpem_codigo,
		pal.paen_altura = pae.paen_altura
from dba.palletencabhisto as pae
WHERE	pae.clie_codigo = :cliente
AND   pae.plde_codigo = :planta
AND   pae.paen_numero = :numero
AND	pae.PAHI_PROCES = :ll_PROCESO
AND	pae.clie_codigo = pal.clie_codigo
AND	pae.plde_codigo = pal.plde_codigo
AND	pae.paen_numero = pal.paen_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Actualizando Tabla PALLETENCAB")
	RETURN False
END IF

UPDATE dba.PALLETFRUTA as pal
SET	PAFR_CCAJAS = PAFH_CCAJAS
from dba.palletfrutahisto as pah, 
WHERE	pah.clie_codigo = :cliente
AND   pah.plde_codigo = :planta
AND   pah.paen_numero = :numero
AND 	pah.PAFH_PROCES = :ll_PROCESO2
AND	pah.clie_codigo = pal.clie_codigo
AND	pah.plde_codigo = pal.plde_codigo
AND	pah.paen_numero = pal.paen_numero
And	pah.espe_codigo = pal.espe_codigo
AND	pah.vari_codigo = pal.vari_codigo
AND	pah.etiq_codigo = pal.etiq_codigo
AND 	pah.emba_codigo = pal.emba_codigo
AND 	pah.prod_codigo = pal.prod_codigo
AND 	pah.PAFH_CALIBR = pal.pafr_calibr;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Actualizando Tabla PALLETFRUTA")
	RETURN False
END IF

//BORRA ULTIMO PROCESO EN LAS TABLAS DE HISTORIA
DELETE FROM dba.palletfrutahisto
WHERE clie_codigo = :cliente
AND   plde_codigo = :planta
AND   paen_numero = :numero
AND 	PAFH_PROCES = :ll_PROCESO2;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Borrando Tabla palletfrutahisto")
	RETURN False
END IF

DELETE FROM dba.palletencabhisto
WHERE clie_codigo = :cliente
AND   plde_codigo = :planta
AND   paen_numero = :numero
AND	PAHI_PROCES = :ll_PROCESO;

Return True
end function

public function long buscanuevofolio (integer planta);Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_actual, ll_fin
Boolean	lb_nulo

li_planta	=	planta

li_movto = 4
Select max(repe_numero) 
Into  :ll_numero
From dba.repalletenca
Where plde_codigo = :li_planta;

Select como_inicia, como_actual, como_termin
Into	:ll_numero2, :ll_actual, :ll_fin
from DBA.correlmovimientos
Where plde_codigo = :li_planta
and	como_tipomv = :li_movto;

IF ll_actual >= ll_fin THEN
	Return 0
END IF	

ll_fin = ll_fin - 3

IF ll_actual >= ll_fin THEN 
	MessageBox("Advertencia","Quedan Menos de 3 Correlativos, proceda por mantención 'Correlativos'")
END IF	

IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla CORRELMOVIMIENTOS")
END IF

IF Isnull(ll_numero) OR String(ll_numero) = '' or ll_numero < ll_numero2 THEN
	ll_numero = ll_numero2
END IF	

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla repalletenca")
ELSEIF sqlca.SQLCode = 0 THEN
	 ll_numero++
END IF

RETURN ll_numero



















end function

public function boolean eliminapallet (long pallet);Integer 	li_cliente,li_planta
Long		ll_palet,ll_palet1,ll_palet3,ll_palet4,ll_palet5,ll_palet6
Boolean	lb_resultado = False

li_planta	=	Integer(istr_mant.argumento[1])
li_cliente	=	Integer(istr_mant.argumento[3])

SELECT count(*) INTO :ll_palet1
FROM dba.despafrigode
WHERE clie_codigo	=	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet3
FROM dba.inspecpaldet
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet4
FROM dba.fumigadet
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO	:ll_palet5
FROM dba.reetidet
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet6
FROM dba.alpalletfruta
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

IF IsNull(ll_palet1) THEN	ll_palet1	=	0
IF IsNull(ll_palet3) THEN	ll_palet3 	= 	0
IF IsNull(ll_palet4) THEN	ll_palet4 	= 	0
IF IsNull(ll_palet5) THEN	ll_palet5 	= 	0
IF IsNull(ll_palet6) THEN	ll_palet6 	= 	0

ll_palet		=	ll_palet1+ll_palet3+ll_palet4+ll_palet5+ll_palet6

IF ll_palet	=	0 THEN
	lb_resultado = False
ELSE
	lb_resultado = True
END IF

RETURN lb_resultado
end function

public function boolean actuaestadopallet ();Integer 	li_cliente, li_planta
Long		ll_numero

li_cliente = Integer(istr_mant.argumento[3])
li_planta  = Integer(istr_mant.argumento[1])
ll_numero  = Long(istr_mant.argumento[47])

/** Cambia Estado del Pallet de Repalletizado a Existente **/
		Update dba.palletencab
		Set    paen_estado  	= 1
		Where  paen_Numero 	= :ll_numero
		and    clie_codigo  	= :li_cliente
		and    plde_codigo  	= :li_planta;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla palletencab")
	RETURN False
ELSE	
	Return True
END IF

end function

public function boolean borrapallet ();Integer 	li_cliente, li_planta
Long		ll_numero

li_cliente = Integer(istr_mant.argumento[3])
li_planta  = Integer(istr_mant.argumento[1])
ll_numero  = Long(istr_mant.argumento[47])

/** Elimina pallet de palletfruta **/
		delete from dba.palletfruta
		Where  paen_Numero 	= :ll_numero
		and    clie_codigo  	= :li_cliente
		and    plde_codigo  	= :li_planta;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla palletfruta")
	RETURN False
END IF

/** Elimina pallet de palletencab **/
		delete from dba.palletencab
		Where  paen_Numero 	= :ll_numero
		and    clie_codigo  	= :li_cliente
		and    plde_codigo  	= :li_planta;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla palletencab")
	RETURN False
ELSE	
	Return True
END IF

end function

event open;call super::open;Long 		ll_numero, ll_fila
Integer 	li_planta

/*
  istr_mant.argumento[1]	= li_planta
  istr_mant.argumento[2]	= nfolio
  istr_mant.argumento[3]	= Cliente
  istr_mant.argumento[4]	= Tarjas transitorias
  istr_mant.argumento[5]	= Mantencion
  istr_mant.argumento[6]	= Tarjas definitivas
  istr_mant.argumento[8]   = Lista de Productores de Folios a Repalletizar
  istr_mant.argumento[11]	= Puchos Ingresados
  istr_mant.argumento[12]	= Pallet Ingresados  
  istr_mant.argumento[13]  = Nro. de Pallet
  istr_mant.argumento[14]  = Tipo de Pantalla; 1 para el repalletizado
  istr_mant.argumento[25]  = Valor de Tipo de Repaletizado
*/
IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_2.GetChild("plde_codigo", dw_planta)

dw_planta.SetTransObject(sqlca)

dw_planta.Retrieve(1)

dw_3.SetTransObject(Sqlca)
dw_3.SetRowFocusIndicator(Hand!)
dw_3.Modify("DataWindow.Footer.Height = 110")
dw_4.SetTransObject(Sqlca)
dw_5.SetTransObject(Sqlca)

buscar	= "Pallet Nuevo:Npaen_numero,Descripción:Svari_nombre"
ordenar	= "Pallet Nuevo:paen_numero,Descripción:vari_nombre"

istr_mant.argumento[3]	=	String(gi_CodExport)
istr_mant.argumento[5]	=	'dw_mues_repalletdeta'
istr_mant.argumento[24] =	''
istr_mant.argumento[14] =  '1'
istr_mant.argumento[25]	=	''
istr_mant.argumento[31]	=	'-1'

gb_Repalletizado = True 

// Para guardar las fecha ingreso antiguas de los pallets
//ids_Palletfruta_pafr_fecing				=	Create DataStore
//ids_Palletfruta_pafr_fecing.DataObject	=	'dw_mues_fechas'				
istr_mant.argumento[32]	=	''		// Lista de fechas Ingreso para palletfruta
istr_mant.argumento[33]	=	''		// Lista de fechas Embalaje para palletfruta

//dw_1.SetFilter("repd_tipood = 2")
dw_1.SetFilter("repd_tipood = 2 "+"or repd_tipood = 3")
dw_1.Filter()








end event

event ue_borra_detalle;Str_Mant		lstr_mant
IF ii_dw = 1 THEN
	IF dw_1.rowcount() < 1 THEN
		dw_1.SetFocus()
		RETURN
	END IF
	
	SetPointer(HourGlass!)
	
	ib_borrar = True
	w_main.SetMicroHelp("Validando la eliminación de detalle...")
	
	Message.DoubleParm = 0
	
	This.TriggerEvent ("ue_validaborrar_detalle")
	
	IF Message.DoubleParm = -1 THEN RETURN
	
	istr_mant.borra	= True
	istr_mant.agrega	= False
	
	istr_mant.argumento[47] = String(dw_1.Object.paen_numero[il_fila])
	
	IF lb_escambiodealtura = False THEN
		IF eliminapallet(Long(istr_mant.argumento[47])) = True THEN 
			MessageBox( "Error", "Pallet con Relación en Otras Tablas, No se Puede Eliminar Pallet.")	
			Return
		ELSE
			lb_borrapallet = True
			lb_eliminapallet = False
		END IF
		
		istr_mant.dw		= dw_1
		
		OpenWithParm(iw_mantencion, istr_mant)
		istr_mant = Message.PowerObjectParm
		IF istr_mant.respuesta = 1 THEN
			IF dw_1.DeleteRow(0) = 1 THEN
				ib_borrar = False
				w_main.SetMicroHelp("Borrando Registro...")
				SetPointer(Arrow!)
			ELSE
				ib_borrar = False
				MessageBox(This.Title,"No se puede borrar actual registro.")
			END IF
//			IF dw_1.RowCount() = 0 THEN 
//				HabilitaEncab(True)
//				pb_eli_det.Enabled = False
//			END IF
		END IF
				
	ELSE	
		istr_mant.dw		= dw_1
		OpenWithParm(iw_mantencion, istr_mant)
		istr_mant = Message.PowerObjectParm
		IF istr_mant.respuesta = 1 THEN
			IF dw_1.DeleteRow(0) = 1 THEN
				ib_borrar = False
				w_main.SetMicroHelp("Borrando Registro...")
				lb_actualizatablas = True
				SetPointer(Arrow!)
				IF dw_2.Object.repe_tipopa[1] = 1 OR dw_2.Object.repe_tipopa[1] = 2 THEN
					dw_4.retrieve(dw_2.Object.plde_codigo[1],dw_2.Object.repe_numero[1],dw_2.Object.clie_codigo[1])
					dw_5.retrieve(dw_2.Object.plde_codigo[1],dw_2.Object.repe_numero[1],dw_2.Object.clie_codigo[1])
					dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
					dw_5.RowsMove(1, dw_5.RowCount(), Primary!, dw_5, 1, Delete!)
					IF dw_5.Update(True, False) = 1 THEN
						IF dw_4.Update(True, False) = 1 THEN
							Commit;
							dw_5.ResetUpdate()
							dw_4.ResetUpdate()
						END IF
					END IF	
					lb_actualizatablas = True
				END IF
				lb_borrapallet = False
				lb_eliminapallet = False
			ELSE
				ib_borrar = False
				MessageBox(This.Title,"No se puede borrar actual registro.")
				lb_actualizatablas = False
			END IF
//			IF dw_1.RowCount() = 0 THEN 
//				HabilitaEncab(True)
//				pb_eli_det.Enabled = False
//			END IF
		END IF
	END IF	
ELSE
	istr_mant.argumento[47] = String(dw_3.Object.paen_numero[il_fila2])
	IF lb_escambiodealtura = False THEN
		IF eliminapallet(Long(istr_mant.argumento[47])) = True THEN 
			IF Messagebox('Borrar','Pallet con Relación en Otras Tablas, Solo Elimina detalle en Repalletizaje', question!, yesno!, 2) = 1 THEN
				lb_eliminapallet = True
			ELSE	
				lb_eliminapallet = False
				Return
			END IF
		ELSE
			lb_borrapallet = False
			lb_eliminapallet = False
		END IF	
				
		IF dw_3.RowCount() < 1 THEN
			dw_3.SetFocus()
			RETURN
		END IF
			
		SetPointer(HourGlass!)
			
		lstr_mant			= istr_mant
		lstr_mant.dw		= dw_3
		lstr_mant.borra	= True
		lstr_mant.agrega	= False
			
		OpenWithParm(iw_mantencion, lstr_mant)
		istr_mant = Message.PowerObjectParm
		IF istr_mant.respuesta = 1 THEN
			IF dw_3.DeleteRow(0) = 1 THEN
				ib_borrar = False	
					
				w_main.SetMicroHelp("Borrando Registro...")
				SetPointer(Arrow!)
			ELSE
				ib_borrar = False
				MessageBox(This.Title,"No se puede borrar actual registro.")
			END IF
//			IF dw_3.RowCount() = 0 THEN 
//				HabilitaEncab(True)
//				pb_eli_det.Enabled = False
//			END IF
		END IF
	END IF
END IF
	
	istr_mant.borra	 = False
IF istr_mant.respuesta = 2 THEN
	lb_borrapallet = False
	lb_eliminapallet = False	
END IF	






end event

event ue_nuevo_detalle;Integer  li_productor
Long		ll_cantar, ll_tardef, ll_repe_numero, ll_numero, ll_fila2, ll_fila3, ll_cont

Str_mant		lstr_mant

istr_mant.borra	= False
istr_mant.agrega	= True

IF lb_escambiodealtura THEN
	//Cambios de altura
	dw_3.Enabled 					= 	False
	istr_mant2.agrega				=	True
	
	istr_mant2.argumento[1]	=	String(dw_2.Object.plde_codigo[1])
	istr_mant2.argumento[3]	=	istr_mant.argumento[3]
	istr_mant2.argumento[2]	=	String(buscanuevofolio(Integer(istr_mant2.argumento[1])))
	istr_mant2.argumento[5]	=	"dw_mues_repalletdeta"
	istr_mant2.argumento[9]	=	String(dw_2.Object.repe_fecrep[1])
	istr_mant2.argumento[45]=	istr_mant.argumento[2]
	istr_mant2.dw					=	dw_1
	istr_mant2.argumento[46]=	String(dw_2.Object.repe_tipopa[1])
	
	OpenWithParm(w_maed_palletencabhistoria, istr_mant2)
	
	IF dw_1.RowCount() > 0 THEN 
		HabilitaEncab(False)
		pb_eliminar.Enabled	= 	FALSE
		pb_buscar.Enabled		=	FALSE
		pb_nuevo.Enabled		=	FALSE
		
		pb_grabar.Enabled		= 	TRUE
		separaportipood()
		dw_1.SetRow(il_fila)
		dw_1.SelectRow(il_fila,True)
	END IF
	
	IF dw_2.GetNextModified(0, Primary!) > 0 THEN
		dw_2.SetItem(1, "repe_fecact", Today())
		dw_2.SetItem(1, "repe_horact", Now())
	END IF
	
ELSE
	IF ii_dw = 1 THEN
		IF dw_3.RowCount() = 0 THEN
			istr_mant.argumento[15] = '1'//cuando es nuevo repalletizado
	
		END IF
	
		ll_cantar	=	dw_2.Object.repe_cantar[1]
		ll_tardef	=	dw_2.Object.repe_tardef[1]
		
		IF dw_1.RowCount() >= (ll_cantar + ll_tardef ) THEN
			MessageBox("Atención", "No puede ingresar más Tarjas.")
		ELSE	
			ListaProductores()
			GeneraListaVariedad()
			
			istr_mant.Argumento[7]	=	"0"
			istr_mant.Argumento[16]	=	"0"
			istr_mant.Argumento[21]	=	String(dw_2.Object.plde_codigo[1])
			
			CuentaTarjas()		
			OpenWithParm(w_maed_palletencab_recepcion, istr_mant)
			
			IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
			
			IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
				pb_grabar.Enabled		= TRUE
			END IF
			
			dw_1.SetRow(il_fila)
			dw_1.SelectRow(il_fila,True)
		END IF
	ELSE
		lstr_mant		    = istr_mant
		lstr_mant.dw	    = dw_3
		
		OpenWithParm(iw_mantencion, lstr_mant)
		
		dw_3.SetRow(il_fila)
		dw_3.SelectRow(il_fila,True)
	
		IF dw_3.RowCount() > 0 AND istr_mant.argumento[25] = '6' THEN
			pb_grabar.Enabled	=	True
		END IF
	END IF
END IF
end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_fila_a
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))
	pb_nuevo.Enabled= True
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		IF dw_2.Object.repe_tipopa[1] = 1 OR dw_2.Object.repe_tipopa[1] = 2 THEN
			lb_escambiodealtura	= TRUE
			dw_3.Enabled 			= FALSE
			dw_1.SetFilter("repd_tipood = 2 "+"or repd_tipood = 3")
			dw_1.Filter()
		ELSE
			dw_3.Enabled 			= True
			dw_1.SetFilter("repd_tipood = 2"+"or repd_tipood = 3")
			dw_1.Filter()
			lb_escambiodealtura  = False
		END IF
		DO
			ll_fila_a	= dw_3.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),0,Integer(istr_mant.argumento[3]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
				Information!, RetryCancel!)
			ELSE
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled	= True
				pb_ins_det.Enabled	= True
				
				IF ll_fila_e > 0 THEN
					istr_mant.argumento[25] = String(dw_2.Object.repe_tipopa[1])
				END IF
				
				IF ll_fila_d > 0 or ll_fila_a > 0 THEN
				   pb_eli_det.Enabled	= True
					pb_imprimir.Enabled	= True
					pb_grabar.Enabled		= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					IF ll_fila_d = dw_2.Object.repe_cantar[1] THEN pb_ins_det.Enabled = False
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_repalletenca.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_5
end on

on w_maed_repalletenca.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
end on

event ue_nuevo;call super::ue_nuevo;Long 		ll_numero, ll_null
Integer 	li_planta

SetNull(ll_null)

HabilitaEncab(True)

dw_3.Reset()
dw_4.Reset()
dw_5.Reset()

dw_4.SetTransObject(SqlCa)
dw_5.SetTransObject(SqlCa)

dw_2.SetItem(1, "clie_codigo", gi_CodExport)
dw_2.SetItem(1, "plde_codigo", gi_CodPlanta)

lb_escambiodealtura		=	False
istr_mant.argumento[1]	=	String(gi_CodPlanta)
istr_mant.argumento[3]	=	String(gi_CodExport)

//istr_mant.argumento[2]=String(buscanuevofolio(dw_2.Object.plde_codigo[1]))

//IF Long(istr_mant.argumento[2]) = 0 THEN
//	MessageBox("Advertencia","No Quedan Correlativos Disponibles, Proceda por Mantención.")
//	Message.DoubleParm = -1
//	Return
//END IF	
//
pb_buscar.Enabled = True




end event

event ue_seleccion();Long		ll_null

SetNull(ll_null)

istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[2]	=	String(dw_2.Object.plde_codigo[1])
istr_busq.argum[3]	=	'1'

OpenWithParm(w_busc_repalletenca, istr_busq)

istr_busq	=	Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= istr_busq.argum[2]
	istr_mant.argumento[2]	= istr_busq.argum[5]
	istr_mant.argumento[3]  = istr_busq.argum[1]
	IF	istr_mant.argumento[2]	<>	""	THEN
		dw_2.SetItem(1, "repe_numero", Long(istr_mant.argumento[2]))
		IF ExisteFolio('repe_numero', String(dw_2.Object.repe_numero[1])) THEN
			dw_2.SetFocus()
			dw_2.SetColumn("repe_numero")
			dw_2.SetItem(1, "repe_numero", ll_null)			
			RETURN
		END IF
	ELSE
		This.TriggerEvent("ue_recuperadatos")
	END IF
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long ll_fila2, ll_fila3 = 1, ll_cont, ll_nuevofolio, ll_filadet, ll_find, ll_pallet, ll_secuen
Integer li_fillas, li_cliente, li_planta

IF isnull(dw_2.Object.repe_numero[1]) or dw_2.Object.repe_numero[1] = 0 THEN
	dw_2.SetItem(1, "repe_numero", Long(istr_mant.argumento[2]))
END IF

FOR ll_filadet = 1 TO dw_1.RowCount()
	li_cliente = dw_1.Object.clie_codigo[ll_filadet]
	li_planta = dw_1.Object.plde_codigo[ll_filadet]
	ll_pallet = dw_1.Object.paen_numero[ll_filadet]
	
	ll_find	=	dw_1.Find ( "clie_codigo = " + String(li_cliente) + &
								" AND paen_numero = " + String(ll_pallet) + &
								" AND plde_codigo = " + String(li_planta), 1, dw_1.RowCount())
	
	IF ll_find > 0 THEN
		ll_secuen	=	dw_1.Object.repe_secuen[ll_filadet]
		IF IsNull(ll_secuen) THEN ll_secuen = 0
		dw_1.Object.repe_secuen[ll_filadet] =  ll_secuen + 1
	ELSE	
		dw_1.Object.repe_secuen[ll_filadet] = 1
	END IF	
NEXT	

FOR ll_filadet = 1 TO dw_3.RowCount()
	li_cliente = dw_3.Object.clie_codigo[ll_filadet]
	li_planta = dw_3.Object.plde_codigo[ll_filadet]
	ll_pallet = dw_3.Object.paen_numero[ll_filadet]
	
	ll_find	=	dw_3.Find ( "clie_codigo = " + String(li_cliente) + &
								" AND paen_numero = " + String(ll_pallet) + &
								" AND plde_codigo = " + String(li_planta), 1, dw_3.RowCount())
	
	IF ll_find > 0 THEN
		ll_secuen	=	dw_3.Object.repe_secuen[ll_filadet]
		IF IsNull(ll_secuen) THEN ll_secuen = 0
		dw_3.Object.repe_secuen[ll_filadet]= ll_secuen + 1
	ELSE	
		dw_3.Object.repe_secuen[ll_filadet]= 1
	END IF	
NEXT		
	
IF NOT lb_escambiodealtura THEN
	dw_1.SetFilter("repd_tipood = 1")
	dw_1.Filter()
	
	dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)
	
	dw_3.RowsCopy(1, dw_3.RowCount(), Primary!, dw_1, 1, Primary!)
	
	dw_1.SetFilter("repd_tipood = 2")
	dw_1.Filter()
	
	dw_1.SetRedraw(True)
	
ELSE	
	ll_cont = dw_1.RowCount()
	IF ll_cont <> 0	 THEN
		dw_4.retrieve(dw_2.Object.plde_codigo[1],Long(istr_mant.argumento[2]),dw_2.Object.clie_codigo[1])
		dw_5.retrieve(dw_2.Object.plde_codigo[1],Long(istr_mant.argumento[2]),dw_2.Object.clie_codigo[1])
	
		dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
		dw_5.RowsMove(1, dw_5.RowCount(), Primary!, dw_5, 1, Delete!)
		
		IF dw_5.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				Commit;
				dw_5.ResetUpdate()
				dw_4.ResetUpdate()
			END IF
		END IF	
			
			IF dw_2.Object.repe_tipopa[1] = 1 OR dw_2.Object.repe_tipopa[1] = 2 THEN
				For ll_fila3 = 1 to dw_1.RowCount()
					ll_fila2	=	dw_5.InsertRow(0)
					dw_5.Object.plde_codigo[ll_fila2]	=	dw_1.Object.plde_codigo[1]
					dw_5.Object.altu_numero[ll_fila2]	=	Long(istr_mant.argumento[2])
					dw_5.Object.clie_codigo[ll_fila2]	=	dw_2.Object.clie_codigo[1]
					dw_5.Object.paen_numero[ll_fila2]   =	dw_1.Object.paen_numero[ll_fila3]
					dw_5.Object.alpf_fecmov[ll_fila2]	=	dw_2.Object.repe_fecrep[1]
				Next
				dw_4.Object.plde_codigo[1]	=	dw_1.Object.plde_codigo[1]
				dw_4.Object.altu_numero[1]	=	long(istr_mant.argumento[2])
				dw_4.Object.clie_codigo[1]	=	dw_1.Object.clie_codigo[1]
				dw_4.Object.altu_fecmov[1]	=	dw_2.Object.repe_fecrep[1]
			END IF
	END IF		
END IF
end event

event ue_modifica_detalle;IF lb_escambiodealtura THEN
	//Cambios de altura
	dw_3.Enabled 					= 	False
	istr_mant2.agrega				=	True
	
	istr_mant2.argumento[1]	=	String(dw_2.Object.plde_codigo[1])
	istr_mant2.argumento[3]	=	istr_mant.argumento[3]
	istr_mant2.argumento[2]	=	String(buscanuevofolio(Integer(istr_mant2.argumento[1])))
	istr_mant2.argumento[5]	=	"dw_mues_repalletdeta"
	istr_mant2.argumento[9]	=	String(dw_2.Object.repe_fecrep[1])
	istr_mant2.dw					=	dw_1
	istr_mant2.argumento[11]	=	String(dw_1.Object.paen_numero[dw_1.GetRow()])
	istr_mant2.argumento[45]=	String(dw_2.Object.repe_numero[1])
	OpenWithParm(w_maed_palletencabhistoria,istr_mant2)
ELSE
	IF ii_dw=1 THEN
		IF dw_1.RowCount() > 0 THEN
			ListaProductores()
			GeneraListaVariedad()
			
			istr_mant.agrega			=	False
			istr_mant.borra			=	False
			istr_mant.argumento[7]	=	""
			istr_mant.argumento[6]	=	String(dw_1.GetItemNumber(dw_1.GetRow(),"paen_numero"))
			
			OpenWithParm(w_maed_palletencab_consulta, istr_mant)
		END IF
	END IF
	
	IF ii_dw=3 THEN	
		IF dw_3.RowCount() > 0 THEN
			istr_mant.agrega			=	False
			istr_mant.borra			=	False
			istr_mant.argumento[7]	=	""
			istr_mant.argumento[6]	=	String(dw_3.GetItemNumber(dw_3.GetRow(),"paen_numero"))
			
			OpenWithParm(w_maed_palletencab_consulta, istr_mant)	
		END IF
	END IF
END IF
end event

event ue_borrar;//IF dw_2.RowCount() < 1 THEN RETURN
//
//SetPointer(HourGlass!)
//dw_1.SetRedraw(False)
//
//dw_1.SetFilter("")
//dw_1.Filter()
//
//ib_borrar = True
//w_main.SetMicroHelp("Validando la eliminación...")
//
//Message.DoubleParm = 0
//
//This.TriggerEvent ("ue_validaborrar")
//
//IF Message.DoubleParm = -1 THEN RETURN
//
//IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)
//
//IF dw_2.DeleteRow(0) = 1 THEN
//		ib_borrar = False
//		w_main.SetMicroHelp("Borrando Registro...")
//		IF wf_actualiza_db(True) THEN
//			w_main.SetMicroHelp("Registro Borrado...")
//			This.TriggerEvent("ue_nuevo")
//         pb_salir.PostEvent(Clicked!)
//			SetPointer(Arrow!)
//			
//			
//		ELSE
//			w_main.SetMicroHelp("Registro no Borrado...")
//		END IF
//		
//ELSE
//	ib_borrar = False
//	MessageBox(This.Title,"No se puede borrar actual registro.")
//END IF
//
//dw_1.SetFilter("repd_tipood = 2")
//dw_1.Filter()
//
//dw_1.SetRedraw(False)
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
		
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
		pb_Grabar.Enabled		=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		pb_Grabar.Enabled		=	True
		pb_ins_det.Enabled	=	True
		pb_eli_det.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_ins_det.Enabled	=	False
	ELSE
		pb_ins_det.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_2.Object.repe_tipopa[1] = 3 THEN
	IF dw_3.RowCount() > 0 AND dw_1.RowCount() > 0 THEN
		IF dw_3.Object.sum_cajas[1] <> dw_1.Object.sum_cajasnuevas[1] THEN
			IF Messagebox('Grabar','El total de Cajas nuevas no cuadra con las Antiguas. ¿Desea Continuar?', question!, yesno!, 2) = 2 THEN
				dw_2.Object.repe_estrep[1] = 0
				Return
			ELSE
				dw_2.Object.repe_estrep[1] = 1
			END IF 	
		ELSE	
			dw_2.Object.repe_estrep[1] = 0
		END IF
	ELSE
		IF Messagebox('Grabar','Faltan Ingresar Pallets(Nuevos o Antiguos). ¿Desea Continuar?', question!, yesno!, 2) = 2 THEN
			dw_2.Object.repe_estrep[1] = 0
			Return
		ELSE
			dw_2.Object.repe_estrep[1] = 0
		END IF 
	END IF	
END IF	

IF dw_2.Object.repe_tipopa[1] = 7 OR dw_2.Object.repe_tipopa[1] = 1 OR  dw_2.Object.repe_tipopa[1] = 2 THEN
	IF dw_3.RowCount() > 0 AND dw_1.RowCount() > 0 THEN
		IF dw_3.Object.sum_cajas[1] <> dw_1.Object.sum_cajasnuevas[1] THEN
			dw_2.Object.repe_estrep[1] = 1
		ELSE
			dw_2.Object.repe_estrep[1] = 0
		END IF	
	ELSE
		dw_2.Object.repe_estrep[1] = 1
	END IF	
END IF

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	IF dw_2.Object.repe_tipopa[1] = 1 OR dw_2.Object.repe_tipopa[1] = 2 THEN
		//pb_eliminar.Enabled	= False
		pb_imprimir.Enabled	= True
	ELSE
		//pb_eliminar.Enabled	= True
		pb_imprimir.Enabled	= True
	END IF	
	IF dw_1.RowCount() > 0 or dw_3.RowCount() > 0 THEN
		pb_eli_det.Enabled = True
	END IF
		
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_repalletenca
integer x = 864
integer y = 952
integer width = 2363
integer height = 964
integer taborder = 90
string title = "Detalle de Pallets"
string dataobject = "dw_mues_repalletdeta"
end type

event dw_1::getfocus;call super::getfocus;IF dw_2.Object.repe_tipopa[1]	= 6 THEN
	ii_dw	= 3
	dw_3.Title	= 'Folios Repalletizar'
	dw_1.Title	= ''
	dw_1.Enabled = False
ELSE
	ii_dw	= 1
	This.Title	= 'Detalle de Pallets'
	dw_3.Title	= ''
END IF



end event

event dw_1::clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
	pb_eli_det.Enabled = True
ELSE
	pb_eli_det.Enabled = False	
END IF

RETURN 0

IF Row > 0 THEN
	il_fila2 = Row
	This.SelectRow(0, False)
	This.SetRow(il_fila2)
	This.SelectRow(Row, True)
	pb_eli_det.Enabled = True
ELSE
	pb_eli_det.Enabled = False
END IF

RETURN 0

end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_repalletenca
string tag = "Ingreso Encabezado"
integer x = 50
integer y = 44
integer width = 2729
integer height = 880
integer taborder = 10
string dataobject = "dw_mant_repalletenca"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
String	ls_columna
Date		ld_nula

SetNull(ll_null)
SetNull(ld_nula)
ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "plde_codigo"
	/*	IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF*/		
		istr_mant.Argumento[1] = Data
		
	CASE "repe_numero"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF			
		
	CASE "repe_cantar"
		istr_mant.argumento[4]	= data

   CASE "repe_tardef"
		istr_mant.argumento[6]	= data
		
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		ELSE
			istr_mant.argumento[3]=data
         dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)
		END IF	

	CASE "repe_tipopa"
		IF integer(data) = 6 OR integer(data) = 4 OR integer(data) = 5 THEN
			Messagebox("Advertencia","Tipo de Repalletizado No Disponible En Esta Opción!")
			Parent.TriggerEvent("ue_nuevo")
		ELSEIF integer(data) = 1 OR integer(data) = 2 THEN
			lb_escambiodealtura	= TRUE
			dw_3.Enabled 			= True
			dw_3.Title				=	''
		ELSE
			lb_escambiodealtura	= FALSE
		END IF
		ii_tiporepa = integer(data)
		istr_mant.argumento[2]=String(buscanuevofolio(dw_2.Object.plde_codigo[1]))
		
		IF Long(Istr_mant.argumento[2]) = 0 THEN
			MessageBox("Advertencia","No Quedan Correlativos Disponibles, Proceda por Mantención.")
			This.SetItem(Row, ls_Columna, Integer(ld_nula))
			Message.DoubleParm = -1
			Return 1
		END IF	

	CASE "repe_fecrep"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF
		
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//IF Row > 0 THEN
//	istr_mant.agrega = False
// istr_mant.argumento[1] = parametro de entrada
//	OpenWithParm(w_mant, istr_mant)
//END IF
//
//RETURN 0
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_repalletenca
integer y = 252
integer taborder = 30
end type

event pb_nuevo::clicked;call super::clicked;dw_1.Reset()
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_repalletenca
integer y = 480
integer taborder = 40
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_repalletenca
integer y = 688
integer taborder = 50
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_repalletenca
integer y = 844
integer taborder = 60
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_repalletenca
integer y = 1156
integer taborder = 70
end type

event pb_salir::clicked;call super::clicked;//IF dw_1.RowCount() > 0 THEN
//   pb_grabar.TriggerEvent(clicked!)
//END IF
//
//call super:: clicked
end event

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_repalletenca
integer x = 3301
integer y = 1484
integer taborder = 100
end type

event pb_ins_det::clicked;call super::clicked;IF dw_1.RowCount() > 0 OR dw_3.RowCount() > 0 THEN
	pb_grabar.Enabled		=	True
END IF
end event

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_repalletenca
integer x = 3301
integer y = 1660
integer taborder = 110
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_repalletenca
integer y = 76
integer taborder = 20
end type

type dw_3 from uo_dw within w_maed_repalletenca
integer x = 55
integer y = 952
integer width = 805
integer height = 964
integer taborder = 80
boolean titlebar = true
string title = "Folios Repalletizar"
string dataobject = "dw_mues_repalletdeta_salida"
boolean hscrollbar = true
boolean livescroll = true
end type

event getfocus;call super::getfocus;ii_dw	= 3
This.Title	= 'Folios Repalletizar'
dw_1.Title	= ''

//IF dw_3.RowCount() > 0 THEN
//	pb_eli_det.Enabled = True
//ELSE
//	pb_eli_det.Enabled = False
//END IF
//
end event

event doubleclicked;IF dw_3.RowCount() = 0 THEN
	Return 0
ELSE
	istr_mant.argumento[13] = String(dw_3.Object.paen_numero[dw_3.GetRow()])
END IF

Parent.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila2 = Row
	This.SelectRow(0, False)
	This.SetRow(il_fila2)
	This.SelectRow(Row, True)
	pb_eli_det.Enabled = True
ELSE
	pb_eli_det.Enabled = False
END IF

RETURN 0

end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

type dw_4 from datawindow within w_maed_repalletenca
boolean visible = false
integer x = 2007
integer y = 2024
integer width = 686
integer height = 400
integer taborder = 120
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_alpalletencab1"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_maed_repalletenca
boolean visible = false
integer x = 434
integer y = 1964
integer width = 1157
integer height = 528
integer taborder = 130
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_alpalletfruta1"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

