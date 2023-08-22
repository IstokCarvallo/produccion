$PBExportHeader$w_mant_deta_movtoenvadeta.srw
$PBExportComments$Mantención Detalle de Movimiento Envases
forward
global type w_mant_deta_movtoenvadeta from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_movtoenvadeta from w_mant_detalle_csd
integer width = 2706
integer height = 1864
string title = "DETALLE DE ENVASES"
end type
global w_mant_deta_movtoenvadeta w_mant_deta_movtoenvadeta

type variables
uo_camarasfrigo		iuo_Camara
uo_CaliCosechero		iuo_CalidadEnvase
uo_TipoEnvases		iuo_Tipo
uo_Envases				iuo_Envases	

DataWindowChild		idwc_TipoEnvase, idwc_Camara, idwc_Calidad,idwc_planta
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public subroutine buscaenvase ()
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_TipoEnvase, ls_CodEnvase, ls_CondEnvase, ls_Calidad

ls_TipoEnvase	=	String(dw_1.Object.enva_tipoen[il_Fila])
ls_CodEnvase	=	String(dw_1.Object.enva_codigo[il_Fila])
ls_CondEnvase	=	String(dw_1.Object.fgme_conenv[il_Fila])
ls_Calidad		=	String(dw_1.Object.cale_calida[il_Fila])

CHOOSE CASE as_Columna
	CASE "enva_tipoen"
		ls_TipoEnvase	=	as_Valor

	CASE "enva_codigo"
		ls_CodEnvase	=	as_Valor

	CASE "fgme_conenv"
		ls_CondEnvase	=	as_Valor

	CASE "cale_calida"
		ls_Calidad		=	as_Valor

END CHOOSE

ll_Fila	=	dw_1.Find("enva_tipoen = " + ls_TipoEnvase + " AND " + &
							"enva_codigo = " + ls_CodEnvase + " AND " + &
							"fgme_conenv = " + ls_CondEnvase + " AND " + &
							"cale_calida = '" + ls_Calidad + "'", &
							1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine buscaenvase ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_1.Object.enva_tipoen[il_Fila])
OpenWithParm(w_busc_envases, lstr_busq)
lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	dw_1.SetColumn("enva_codigo")
	dw_1.Object.enva_codigo[il_Fila]	=	Integer(ls_Nula)
	dw_1.Object.enva_nombre[il_Fila]	=	ls_Nula
	dw_1.SetFocus()
ELSE
	dw_1.Object.enva_tipoen[il_Fila]	=	Integer(lstr_busq.argum[1])
	dw_1.Object.enva_codigo[il_Fila]	=	Integer(lstr_busq.argum[2])
	dw_1.Object.enva_nombre[il_Fila]	=	lstr_busq.argum[3]
	dw_1.Object.fgme_pesone[il_Fila]	=	Dec(lstr_busq.Argum[4])
	
	iuo_Envases.Existe(dw_1.object.enva_tipoen[il_fila], dw_1.object.enva_codigo[il_fila], True, Sqlca)
					
	IF idwc_Calidad.Retrieve(iuo_Envases.Tipo, iuo_Envases.Envase) = 0 THEN
		idwc_Calidad.InsertRow(0)
		MessageBox("Atención", "No se han registrado Calidades para " + &
						"Envase indicado.~r~rIngrese o seleccione otro Envase")
	ELSE
		idwc_Calidad.SetSort("cale_nombre A")
		idwc_Calidad.Sort()
	END IF
END IF
end subroutine

on w_mant_deta_movtoenvadeta.create
call super::create
end on

on w_mant_deta_movtoenvadeta.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[2]	=	String(dw_1.object.enva_tipoen[il_fila])
ias_campo[3]	=	String(dw_1.object.enva_codigo[il_fila])
ias_campo[4]	=	String(dw_1.object.fgme_cantid[il_fila])
ias_campo[5]	=	String(dw_1.object.fgme_pesone[il_fila])
ias_campo[6]	=	String(dw_1.object.clie_codigo[il_fila])

IF istr_mant.Agrega THEN
	dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
	dw_1.Object.meen_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
	dw_1.Object.fgme_sentid[il_Fila]	=	Integer(istr_Mant.Argumento[4])
   	dw_1.Object.clie_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[16])
	dw_1.Object.prod_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[17])
	dw_1.Object.prod_nombre[il_Fila]	=	String(istr_Mant.Argumento[18])
	
	IF istr_Mant.Argumento[7] <> '1' THEN
		dw_1.SetItem(il_fila,"fgme_conenv",1)
	ELSE
		dw_1.SetItem(il_fila,"fgme_conenv",0)
	END IF	
	
	IF istr_mant.Argumento[7] <> '' THEN
		dw_1.Object.fgme_conenv.Protect		=	1
	END IF	

ELSE
	iuo_Envases.Existe(dw_1.object.enva_tipoen[il_fila], dw_1.object.enva_codigo[il_fila],  True, Sqlca)

	dw_1.Object.clie_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[16])
	
	IF idwc_Calidad.Retrieve(iuo_Envases.Tipo, iuo_Envases.Envase) = 0 THEN
		idwc_Calidad.InsertRow(0)
		
		MessageBox("Atención", "No se han registrado Calidades para " + &
						"Envase indicado.~r~rIngrese o seleccione otro Envase")
	ELSE
		idwc_Calidad.SetSort("cale_nombre A")
		idwc_Calidad.Sort()
	END IF
					 
END IF
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.object.enva_tipoen[il_Fila]	=	Integer(ias_campo[2])
	dw_1.object.enva_codigo[il_Fila]	=	Integer(ias_campo[3])
	dw_1.object.fgme_cantid[il_Fila]	=	Integer(ias_campo[4])
	dw_1.object.fgme_pesone[il_Fila]	=	Dec(ias_campo[5])
END IF
end event

event ue_antesguardar();Integer	li_Contador
String	ls_Mensaje, ls_Columna[]


IF IsNull(dw_1.Object.enva_codigo[il_Fila]) OR &
	dw_1.Object.enva_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCódigo de Envase"
	ls_Columna[li_Contador]	=	"enva_codigo"
END IF

IF IsNull(dw_1.Object.fgme_conenv[il_Fila]) THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nEstado del Envase"
	ls_Columna[li_Contador]	=	"fgme_conenv"
END IF

IF IsNull(dw_1.Object.cale_calida[il_Fila]) OR &
	dw_1.Object.cale_calida[il_Fila] = "" THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCalidad del Envase"
	ls_Columna[li_Contador]	=	"cale_calida"
END IF

IF IsNull(dw_1.Object.fgme_cantid[il_Fila]) OR &
	dw_1.Object.fgme_cantid[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCantidad"
	ls_Columna[li_Contador]	=	"fgme_cantid"
END IF
//
//IF IsNull(dw_1.Object.fgme_pesone[il_Fila]) OR &
//	dw_1.Object.fgme_pesone[il_Fila] = 0 THEN
//	li_Contador ++
//	ls_Mensaje 					+=	"~nPeso Neto"
//	ls_Columna[li_Contador]	=	"fgme_pesone"
//END IF


IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
dw_1.Object.meen_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
dw_1.Object.fgme_sentid[il_Fila]	=	Integer(istr_Mant.Argumento[4])
dw_1.Object.clie_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[16])

IF istr_Mant.Argumento[7]	=	'1' THEN
	dw_1.Object.enva_tipoen[il_Fila]	=	Integer(istr_Mant.Argumento[8])
	dw_1.Object.enva_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[9])
	dw_1.Object.enva_nombre[il_Fila]	=	istr_Mant.Argumento[10]
	dw_1.SetItem(il_fila,"fgme_conenv",0)
ELSE
	dw_1.SetItem(il_fila,"fgme_conenv",1)
END IF	

IF istr_mant.Argumento[7] <> '' THEN
	dw_1.Object.fgme_conenv.Protect		=	1
END IF	

dw_1.SetColumn("enva_tipoen")
end event

event open; Integer	li_Null
x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

iuo_Camara			=	Create uo_camarasfrigo
iuo_CalidadEnvase	=	Create uo_CaliCosechero
iuo_Tipo				=	Create uo_TipoEnvases
iuo_Envases			=	Create uo_Envases

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(gi_codexport)

dw_1.GetChild("enva_tipoen", idwc_TipoEnvase)
idwc_TipoEnvase.SetTransObject(sqlca)
idwc_TipoEnvase.Retrieve(-1)
idwc_TipoEnvase.SetSort("tien_nombre A")
idwc_TipoEnvase.Sort()

idwc_TipoEnvase.SetFilter("tien_usoenv = 1")
idwc_TipoEnvase.Filter()

dw_1.GetChild("cale_calida", idwc_Calidad)
idwc_Calidad.SetTransObject(sqlca)

IF idwc_Calidad.Retrieve(-1,-1) = 0 THEN
	idwc_Calidad.InsertRow(0)
ELSE
	idwc_Calidad.SetSort("cale_nombre A")
	idwc_Calidad.Sort()
END IF

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_movtoenvadeta
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_movtoenvadeta
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_movtoenvadeta
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_movtoenvadeta
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_movtoenvadeta
integer x = 2331
integer y = 496
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_movtoenvadeta
integer x = 2336
integer y = 232
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_movtoenvadeta
integer x = 2341
integer y = 800
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_movtoenvadeta
integer x = 91
integer y = 96
integer width = 2075
integer height = 1644
string dataobject = "dw_mant_movtoenvadeta"
end type

event dw_1::itemchanged;String  ls_Columna, ls_Nula
Long    ll_Suma, ll_Fila
Integer li_TipoEnvase, li_CodigoEnvase
Dec{2}	 ld_Peso

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "enva_tipoen"
		IF Not iuo_Tipo.Existe(Integer(Data), True, Sqlca) OR Duplicado(ls_Columna, Data) OR iuo_Tipo.Uso <> 1 THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSE
			li_CodigoEnvase	=	This.Object.enva_codigo[il_Fila]
			istr_Mant.Argumento[8]	=	Data
			
			IF Not IsNull(li_CodigoEnvase) AND li_CodigoEnvase > 0  THEN
				idwc_Calidad.SetTransObject(sqlca)
				IF idwc_Calidad.Retrieve(Integer(Data), li_CodigoEnvase) = 0 THEN
					idwc_Calidad.InsertRow(0)
					
					MessageBox("Atención", "No se han registrado Calidades para " + &
									"Envase indicado.~r~rIngrese o seleccione otro Envase")
									
					This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
					RETURN 1
				ELSE
					idwc_Calidad.SetSort("cale_nombre A")
					idwc_Calidad.Sort()
				END IF
			END IF
		END IF

	CASE "enva_codigo"
		istr_Mant.Argumento[8]	= String(this.Object.enva_tipoen[il_fila])
		IF Not iuo_Envases.existe(iuo_Tipo.Codigo, Integer(Data), True, Sqlca) OR &
			Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSE
			dw_1.Object.enva_nombre[il_Fila]	=	iuo_Envases.Nombre
			dw_1.Object.fgme_pesone[il_Fila]	=	iuo_Envases.Neto
			li_TipoEnvase							=	This.Object.enva_tipoen[il_Fila]
			istr_Mant.Argumento[9]				=	Data
			istr_Mant.Argumento[10]			=	iuo_Envases.Nombre
			IF Not IsNull(li_TipoEnvase) AND li_TipoEnvase > 0  THEN
				IF idwc_Calidad.Retrieve(li_TipoEnvase, Integer(Data)) = 0 THEN
					idwc_Calidad.InsertRow(0)
					
					MessageBox("Atención", "No se han registrado Calidades para " + &
									"Envase indicado.~r~rIngrese o seleccione otro Envase")
									
					This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
					RETURN 1
				ELSE
					idwc_Calidad.SetSort("cale_nombre A")
					idwc_Calidad.Sort()
				END IF
			END IF
		END IF
		
	CASE "fgme_conenv"
		IF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		
	CASE "cale_calida"
		IF Not iuo_CalidadEnvase.Existe(iuo_Envases.Tipo, iuo_Envases.Envase, Data, True, SQLCA) OR &
			Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, ls_Nula)
			RETURN 1
		ELSE
			dw_1.Object.cale_calida[il_Fila]	=	iuo_CalidadEnvase.Calidad
			dw_1.Object.cale_nombre[il_Fila]	=	iuo_CalidadEnvase.Nombre
		END IF
		
	CASE "fgme_cantid"
		IF Dec(data) > 999999 OR Dec(data) < 0 THEN
			This.SetItem(row, "fgme_cantid", Dec(ls_Nula))
			RETURN 1
		END IF
		
	CASE "fgme_pesone"
		IF Dec(data) > 100000 AND Dec(data) < 0 THEN
			dw_1.Object.fgme_pesone[row] = Dec(ls_nula)
			RETURN
		END IF
		
END CHOOSE
end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "b_buscaenvase"
		BuscaEnvase()

END CHOOSE
end event

