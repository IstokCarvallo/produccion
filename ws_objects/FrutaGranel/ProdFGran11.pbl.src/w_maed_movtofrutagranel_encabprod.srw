$PBExportHeader$w_maed_movtofrutagranel_encabprod.srw
$PBExportComments$Recepción de Fruta Granel de Huerto
forward
global type w_maed_movtofrutagranel_encabprod from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_movtofrutagranel_encabprod
end type
type dw_4 from datawindow within w_maed_movtofrutagranel_encabprod
end type
end forward

global type w_maed_movtofrutagranel_encabprod from w_mant_encab_deta_csd
integer width = 3525
integer height = 2008
string menuname = ""
boolean minbox = false
boolean maxbox = false
dw_3 dw_3
dw_4 dw_4
end type
global w_maed_movtofrutagranel_encabprod w_maed_movtofrutagranel_encabprod

type variables
DataWindowChild		idwc_Transp,idwc_Camion,idwc_Variedad,idwc_Predio,&
							idwc_Camara,idwc_especie,idwc_especiedet,idwc_especiedet1,idwc_especiedet2,&
							idwc_planta,idwc_plantadw4,idwc_plancodw4, idwc_plantadw1,idwc_Cliente

Str_mant					istr_mant3, istr_mant4
uo_transportista		iuo_Transport
uo_camiones			iuo_Camion
uo_especie				iuo_Especie
uo_Productores			iuo_Productor
uo_LotesCorrel			iuo_Correlativo
uo_pesoestanespe		iuo_PesoEstanEspe
uo_fechaMovto			iuo_FechaMovto
uo_tipomovtofruta		iuo_TipoMovtoFruta
uo_tipomovtofruta		iuo_TipoMovtoEnva
uo_bins					iuo_bins
uo_calicosechero  	iuo_calicosechero
uo_manejoimpresora	iuo_impresoras

integer					ii_kildec
String						is_rut, is_chofer, is_rutprod
Long						il_NumFruta
DateTime				idt_FechaSistema
end variables

forward prototypes
public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, long al_numero, integer ai_cliente)
public function boolean noexistechofer (string rut)
public function boolean noexistecliente (integer ai_cliente)
public subroutine buscaproductor (long productor)
protected subroutine habilitaingreso (string as_columna)
public subroutine buscacamion ()
public subroutine habilitaencab (boolean habilita)
end prototypes

public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, long al_numero, integer ai_cliente);Long		ll_Numero
Boolean	lb_Retorno = True

SELECT	mfge_numero
	INTO	:ll_Numero
	FROM	dbo.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfge_numero	=	:al_Numero
	AND   clie_codigo =  :ai_Cliente
	AND   IsNull(defg_tipdoc, -1) = -1;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recepción Fruta Granel")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[3]	=	String(al_Numero)
	This.TriggerEvent("ue_recuperadatos")
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado o ~r~nNo es una " + Lower(This.Title) + ". Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean noexistechofer (string rut);String ls_nombre,ls_paterno,ls_materno

SELECT 	clpr_nombre,clpr_apepat,clpr_apemat
INTO		:ls_nombre, :ls_paterno , :ls_materno
FROM 		dbo.clienprove
WHERE		clpr_nrorut =:rut;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClienProve")
	Return True
//ELSEIF sqlca.SQLCode = 100 THEN
//	MessageBox("Atención","Rut No ha sido Generado. Ingrese Otro.")
//	is_chofer	= 	""
//	Return False
END IF

is_chofer	= 	ls_nombre + " " + ls_paterno + " " + ls_materno
Return False
end function

public function boolean noexistecliente (integer ai_cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dbo.clientesprod
WHERE		clie_codigo =:ai_cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public subroutine buscaproductor (long productor);Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	''

OpenWithParm(w_busc_productores, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_1.SetColumn("prod_codigo")
	dw_1.SetFocus()
ELSE
	dw_1.Object.prod_codigo[il_fila]	=	Long(lstr_busq.argum[1])
	dw_1.Object.prod_nombre[il_fila]	=	lstr_busq.argum[2]
	is_rutprod								=	lstr_busq.argum[8]
	
	dw_1.SetFocus()
END IF

RETURN
end subroutine

protected subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_hora
Integer	li_Estado

li_estado	=	dw_2.Object.mfge_estmov[1]

IF Isnull(li_estado) THEN li_estado = 1

IF as_Columna <> "mfge_fecmov" AND &
	(dw_2.Object.mfge_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.mfge_fecmov[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "refg_horaen" AND &
	(dw_2.Object.refg_horaen[1] = lt_hora OR IsNull(dw_2.Object.refg_horaen[1])) THEN
	lb_Estado = False
END IF
/*
IF as_Columna <> "tran_codigo" AND &
	(dw_2.Object.tran_codigo[1] = 0 OR IsNull(dw_2.Object.tran_codigo[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "cami_patent" AND &
	(dw_2.Object.cami_patent[1] = "" OR IsNull(dw_2.Object.cami_patent[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "mfge_chofer" AND &
	(dw_2.Object.mfge_chofer[1] = "" OR IsNull(dw_2.Object.mfge_chofer[1])) THEN
	lb_Estado = False
END IF
*/
	
IF li_Estado = 3 THEN 
	lb_Estado	=	False
END IF

pb_ins_det.Enabled	=	lb_estado
//HabilitaEncab(Not lb_estado)
end subroutine

public subroutine buscacamion ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = '1'

OpenWithParm(w_busc_camiones, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("cami_patent")
	dw_2.SetFocus()
ELSE
	dw_2.Object.cami_clasifi[il_fila]	=	Integer(lstr_busq.argum[1])
	dw_2.Object.cami_patent[il_fila]		=	lstr_busq.argum[2]
	dw_2.Object.cami_patcar[il_fila]		=	lstr_busq.argum[6]
	dw_2.Object.mfge_rutcho[il_fila]		=	lstr_busq.argum[5]
	is_rut = lstr_busq.argum[5]
	dw_2.Object.mfge_chofer[il_fila]		=	lstr_busq.argum[4]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.mfge_numero.Protect				=	0
	dw_2.Object.espe_codigo.Protect					=	0
	dw_2.Object.tran_codigo.Protect					=	0
	dw_2.Object.cami_patent.Protect					=	0
	dw_2.Object.cami_patcar.Protect					=	0
	dw_2.Object.mfge_rutcho.Protect					=	0
	dw_2.Object.mfge_chofer.Protect					=	0
	dw_2.Object.mfge_totbul.Protect					=	0
	dw_2.Object.mfge_fecmov.Protect					=	0
	dw_2.Object.refg_horaen.Protect					=	0
	dw_2.Object.mfge_totbul.Protect					=	0
	dw_2.Object.mfge_observ.Protect					=	0
	dw_2.Object.clie_codigo.Protect					=	0
	
	dw_2.Object.mfge_numero.Color	=	0
	dw_2.Object.espe_codigo.Color	=	0
	dw_2.Object.tran_codigo.Color		=	0	
	dw_2.Object.cami_patent.Color	=	0
	dw_2.Object.cami_patcar.Color	=	0
	dw_2.Object.mfge_rutcho.Color	=	0
	dw_2.Object.mfge_chofer.Color	=	0
	dw_2.Object.mfge_totbul.Color		=	0	
	dw_2.Object.mfge_fecmov.Color	=	0
	dw_2.Object.refg_horaen.Color	=	0
	dw_2.Object.mfge_totbul.Color		=	0
	dw_2.Object.mfge_observ.Color	=	0
	dw_2.Object.clie_codigo.Color		=	0
	
	
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patent.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patcar.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_chofer.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_totbul.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.mfge_fecmov.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_horaen.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.mfge_totbul.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.mfge_observ.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)	
	
	dw_2.Object.refg_horasa.Protect					=	1
	dw_2.Object.refg_fecsal.Protect					=	1
	dw_2.Object.refg_horasa.Color						=	RGB(255,255,255)	
	dw_2.Object.refg_fecsal.Color						=	RGB(255,255,255)
	dw_2.Object.refg_horasa.BackGround.Color		=	553648127
	dw_2.Object.refg_fecsal.BackGround.Color		=	553648127
	
ELSE
	dw_2.Object.mfge_numero.Protect				=	1
	dw_2.Object.espe_codigo.Protect					=	1
	dw_2.Object.tran_codigo.Protect					=	1
	dw_2.Object.cami_patent.Protect					=	1
	dw_2.Object.cami_patcar.Protect					=	1
	dw_2.Object.mfge_rutcho.Protect					=	1
	dw_2.Object.mfge_chofer.Protect					=	1
	dw_2.Object.clie_codigo.Protect					=	1
	
	dw_2.Object.mfge_numero.Color	=	RGB(255,255,255)
	dw_2.Object.espe_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.tran_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patent.Color	=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_chofer.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.Color		=	RGB(255,255,255)
	
	dw_2.Object.mfge_numero.BackGround.Color	=	553648127
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.mfge_rutcho.BackGround.Color	=	553648127
	dw_2.Object.mfge_chofer.BackGround.Color	=	553648127
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	
	dw_2.Object.refg_horasa.Protect					=	1
	dw_2.Object.refg_fecsal.Protect					=	1
	dw_2.Object.mfge_fecmov.Protect					=	1
	dw_2.Object.refg_horaen.Protect					=	1
	dw_2.Object.mfge_totbul.Protect					=	1
	dw_2.Object.mfge_observ.Protect					=	1
	
	dw_2.Object.refg_horasa.Color		=	RGB(255,255,255)
	dw_2.Object.refg_fecsal.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_fecmov.Color	=	RGB(255,255,255)
	dw_2.Object.refg_horaen.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_totbul.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_observ.Color	=	RGB(255,255,255)
	
	dw_2.Object.refg_horasa.BackGround.Color		=	553648127
	dw_2.Object.refg_fecsal.BackGround.Color		=	553648127
	dw_2.Object.mfge_fecmov.BackGround.Color	=	553648127	
	dw_2.Object.refg_horaen.BackGround.Color		=	553648127	
	dw_2.Object.mfge_totbul.BackGround.Color		=	553648127	
	dw_2.Object.mfge_observ.BackGround.Color	=	553648127	
END IF

dw_2.Object.buscacamion.Enabled	=	NOT gb_RecepcionDeProceso
end subroutine

event open;Integer	li_resultado
String	ls_parametros, ls_nombre

x				=	0
y				=	0
This.Height	=	2500
im_menu		=	m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

IF NOT gb_RecepcionDeProceso THEN
	This.Title = "RECEPCION DE HUERTO"
ELSE
	This.Title = "RECEPCION DE PREPROCESO"
END IF

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

istr_mant.argumento[02]	=	'1'
istr_mant.argumento[01]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[04]	=	'1'
istr_mant.argumento[05]	=	String(gstr_ParamPlanta.CodigoEspecie)
istr_mant.argumento[07]	=	'0'
istr_mant.argumento[09]	=	'R'
istr_mant.argumento[10] =	String(gi_codexport)
il_NumFruta					=	0

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve() 

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
ELSE
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
END IF

dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

IF idwc_Transp.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Transportistas")
	idwc_Transp.InsertRow(0)
ELSE
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()	
END IF

istr_mant.dw						=	dw_2
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.solo_consulta			=	False

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso,  This.Title, "Acceso a Aplicación", 1)
							
iuo_Transport				=	Create uo_transportista
iuo_Camion					=	Create uo_camiones
iuo_Especie					=	Create uo_especie
iuo_Correlativo				=	Create uo_LotesCorrel
iuo_PesoEstanEspe		=	Create uo_PesoEstanEspe
iuo_FechaMovto			=	Create uo_FechaMovto
iuo_tipomovtofruta			=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva		=	Create uo_tipomovtofruta
iuo_bins						=	Create uo_bins
iuo_calicosechero			=  Create uo_calicosechero
iuo_Productor				=	Create uo_Productores
end event

event ue_antesguardar;call super::ue_antesguardar;Boolean     lb_Actualiza_Fruta = FALSE, lb_Actualiza_Envase = FALSE
Integer 	 	li_TipoMovto, li_TipoMovtoEnva, li_Planta, li_pesaje, &
				li_Lote_pltcod, li_Lote_espcod, li_Lote_codigo, li_Secuencia, &
				li_TipoEnvase, li_Envase, li_Lote_Ant, li_camara, li_Cliente
Long 			ll_Filas, ll_Total_Bultos, ll_Envases_Fruta, ll_Fila, ll_Fila_Busca, &
  				ll_Fila_d, ll_Primer_NumEnva, ll_Productor, ll_Numero

Message.DoubleParm = 0

li_Planta					=	Integer(istr_mant.Argumento[1])
li_TipoMovto				=	Integer(istr_mant.Argumento[2])
li_TipoMovtoenva  		=  41  	  //Recepción de Envases
li_Cliente					=	Integer(istr_mant.Argumento[10])
 
IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
	
	IF il_NumFruta=0 THEN
		iuo_TipoMovtoFruta.bloqueacorrel()
		il_NumFruta = iuo_TipoMovtoFruta.UltimoCorrelativo(1,li_TipoMovto,li_Planta) 
		
		IF il_NumFruta = 0 OR IsNull(il_NumFruta) THEN
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			Return
		ELSE
			lb_Actualiza_Fruta = TRUE	
		END IF
	END IF
	IF lb_Actualiza_Fruta  THEN 
		iuo_TipoMovtoFruta.Actualiza_Correlativo(1,li_Planta,li_TipoMovto,il_NumFruta) 
	END IF
	dw_2.Object.mfge_numero[1]	=	il_NumFruta
	istr_mant.argumento[3]		=	String(il_NumFruta)
END IF

FOR ll_Filas = dw_1.RowCount() TO 1 Step -1
	IF dw_1.GetItemStatus(ll_filas, 0, Primary!) = New! THEN
		dw_1.DeleteRow(ll_filas)
	ELSE
		dw_1.Object.plde_codigo[ll_filas]	=	li_planta
		dw_1.Object.tpmv_codigo[ll_filas]	=	li_tipomovto
		dw_1.Object.clie_codigo[ll_filas]	=	li_cliente
		dw_1.Object.mfge_numero[ll_filas]	=	il_NumFruta
	END IF
NEXT

IF dw_1.RowCount() < 1 THEN
	Message.DoubleParm = -1
	Return
END IF
end event

on w_maed_movtofrutagranel_encabprod.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
end on

on w_maed_movtofrutagranel_encabprod.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
end on

event ue_nuevo;Long			ll_modif
Time 			lt_hora

is_rut		=	''
is_rutprod	=	''

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_2.GetNextModified(0, Primary!)
		
			IF dw_2.RowCount() > 0 and ll_modif > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_2.Reset()
dw_2.SetRedraw(False)
dw_2.InsertRow(0)
dw_2.SetRedraw(True)
dw_1.Reset()

dw_2.SetFocus()

dw_2.Enabled						=	True
pb_grabar.Enabled					=	False
pb_ins_det.Enabled				=	False
pb_eli_det.Enabled				=	False
pb_eliminar.Enabled				=	False
pb_imprimir.Enabled				=	False

idt_FechaSistema					=	F_FechaHora()
lt_hora								=	Time(F_FechaHora())

dw_2.Object.tpmv_codigo[1]		=	1
dw_2.Object.refg_horaen[1]		=	lt_hora
dw_2.Object.clie_codigo[1]		=	gi_CodExport
dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.espe_codigo[1]		=	gstr_ParamPlanta.CodigoEspecie
dw_2.Object.mfge_fecmov[1]		=	Date(String(idt_FechaSistema,'dd/mm/yyyy'))

istr_Mant.Argumento[3]			=	''
istr_mant.Argumento[5]			=	String(dw_2.Object.espe_codigo[1])
istr_mant.Argumento[7]			=	'0'

il_NumFruta							=	0

IF iuo_especie.existe(gstr_ParamPlanta.CodigoEspecie,TRUE,SQLCA) THEN
	IF iuo_especie.kildec = 1 THEN
		ii_kildec = 2
	ELSE
		ii_kildec = 0
	END IF
END IF

HabilitaEncab(True)
		
dw_2.SetColumn("mfge_numero")
dw_2.SetFocus()
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Integer	li_fila

li_fila	=	dw_1.InsertRow(0)
dw_1.ScrollToRow(li_fila)
dw_1.SetRow(li_fila)

IF li_fila > 0 THEN
	pb_eli_det.Enabled	=	True
ELSE
	pb_eli_det.Enabled	=	False
END IF

pb_grabar.Enabled						=	True
dw_1.Object.mfge_feccos[li_fila]	=	Date(F_FechaHora())

dw_1.SetColumn("prod_codigo")
dw_1.SetFocus()
end event

event ue_borra_detalle;call super::ue_borra_detalle;Integer	li_fila

dw_1.DeleteRow(dw_1.GetRow())
li_fila	=	dw_1.RowCount()

IF li_fila > 0 THEN
	pb_eli_det.Enabled	=	True
ELSE
	pb_eli_det.Enabled	=	False
END IF
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_e, respuesta

w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

pb_eliminar.Enabled		= False
pb_grabar.Enabled		= False
pb_eli_det.Enabled		= False

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),&
										 Long(istr_mant.argumento[3]),Integer(istr_mant.argumento[10]))
	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila_e > 0 Then
		is_rut = dw_2.Object.mfge_rutcho[1]
		istr_mant.Argumento[5]	=	String(dw_2.Object.espe_codigo[1])
		dw_2.SetRedraw(True)
		
//		If Not gb_RecepcionDeProceso Then
//			iuo_Camion.Existe(1, dw_2.Object.cami_patent[1], True, sqlca)
//		End If
		
		If dw_2.Object.mfge_estmov[1] = 3 Then 
			istr_mant.Solo_Consulta	=	True
		Else
			istr_mant.Solo_Consulta	=	False
		End If
		
		DO
			If dw_1.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),Integer(istr_mant.argumento[10])) = -1 Then
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Else
				pb_eliminar.Enabled  = Not istr_mant.Solo_Consulta
				pb_grabar.Enabled	= Not istr_mant.Solo_Consulta
				pb_ins_det.Enabled	= Not istr_mant.Solo_Consulta
				pb_eli_det.Enabled	= Not istr_mant.Solo_Consulta
				pb_imprimir.Enabled	= True
				HabilitaEncab(False)
				dw_1.SetRow(1)
				dw_1.SetFocus()
			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1
							
If respuesta = 2 Then Close(This)
end event

event ue_imprimir;SetPointer(HourGlass!)
Long		fila

//iuo_impresoras			=	Create uo_manejoimpresora
//
dw_4.setTransObject(sqlca)
fila = dw_4.Retrieve(Long(istr_mant.argumento[1]),&
						   Long(istr_mant.argumento[2]),&
							Long(istr_mant.argumento[3]),&
							Integer(istr_mant.argumento[10]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
//	IF iuo_impresoras.setimprcomp() <> -1 THEN 
		dw_4.Print(False, False)
//		iuo_impresoras.setimprdef()
//	ELSE
//		MessageBox("Error", "Ha ocurrido un problema al tratar de imprimir")
//	END IF
END IF
//
//Destroy iuo_impresoras;
		
//istr_info.titulo	= "INFORME ENCABEZADO RECEPCIÓN"
//istr_info.copias	= 1
//
//OpenWithParm(vinf,istr_info)
//
//vinf.dw_1.DataObject = "dw_info_recepcion_enca"
//
//vinf.dw_1.SetTransObject(sqlca)
//
//fila = vinf.dw_1.Retrieve(Long(istr_mant.argumento[1]),&
//								  Long(istr_mant.argumento[2]),&
//								  Long(istr_mant.argumento[3]),&
//								  Integer(istr_mant.argumento[10]))
//
//IF fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF fila = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF

SetPointer(Arrow!)
end event

event ue_seleccion;call super::ue_seleccion;IF ib_ok = False THEN RETURN

Str_Busqueda	lstr_busq

lstr_busq.argum[01] = String(gstr_ParamPlanta.CodigoPlanta)
lstr_busq.argum[02] = '1'								// Movimiento de Recepción
lstr_busq.argum[03] = ''									// Cualquier estado
lstr_busq.argum[04] = String(idt_FechaSistema)	// Desde Fecha de Inicio Ducha
lstr_busq.argum[05] = istr_mant.argumento[5]	// Especie
lstr_busq.argum[10] = istr_mant.argumento[10]	// Codigo Cliente

OpenWithParm(w_busc_spro_movtofrutagranenca_recepcion, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_mant.Argumento[1]	=	lstr_busq.argum[1]
	istr_mant.Argumento[2]	=	lstr_busq.argum[2]
	istr_mant.argumento[3]	=	lstr_busq.argum[3]
	istr_mant.argumento[10]	=	lstr_busq.argum[10]
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutagranel_encabprod
integer x = 507
integer y = 924
integer width = 2066
integer height = 944
string title = "Detalle de Guías del Productor"
string dataobject = "dw_mant_mues_movtofrutagranprod"
end type

event dw_1::itemchanged;call super::itemchanged;String   ls_columna
Integer	li_null, li_Cantidad, li_fila

SetNull(li_null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "prod_codigo"
		IF Not iuo_Productor.Existe(Long(Data),True, sqlca) THEN
			This.SetItem(Row, ls_Columna, li_Null)
			This.SetItem(Row, "prod_nombre", String(li_Null))
			RETURN 1
		ELSE
			This.Object.prod_nombre[Row]	=	iuo_Productor.Nombre
		END IF
		
	CASE "mfge_feccos"
		IF Date(Data) > Date(F_FechaHora()) THEN
			MessageBox("Error", "La fecha de cosecha no puede ser superior a la fecha actual", StopSign!)
			This.Object.mfge_feccos[Row]	=	Date(F_FechaHora())
			Return 1
		END IF
		
END CHOOSE
end event

event dw_1::buttonclicked;call super::buttonclicked;String   ls_columna
Integer	li_null, li_Cantidad, li_fila

SetNull(li_null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "b_prod"
		buscaproductor(Integer(istr_mant.argumento[10]))	
		
END CHOOSE
end event

event dw_1::doubleclicked;//
end event

event dw_1::losefocus;//
end event

event dw_1::itemerror;Return 1
end event

event dw_1::getfocus;//
end event

event dw_1::dwnkey;//
end event

event dw_1::clicked;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::ue_seteafila;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutagranel_encabprod
integer width = 2821
integer height = 824
string dataobject = "dw_mant_movtofrutagranenca_separada"
end type

event dw_2::itemchanged;call super::itemchanged;String	ls_Nula
String	ls_Columna,ls_pat
Date		ld_Fecha
Time		lt_Hora
 
ls_Columna = dwo.Name
SetNull(ls_Nula)
 
CHOOSE CASE ls_Columna
		
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			RETURN 1
		ELSE
			This.GetChild("espe_codigo", idwc_especie)
			idwc_especie.SetTransObject(sqlca)
			idwc_especie.Retrieve(integer(data))
			istr_mant.Argumento[10] = data
			This.SetItem(row, ls_Columna, Integer(Data))
			This.AcceptText()
		END IF	
	
	CASE "mfge_numero"
		IF NOT ExisteRecepcion(gstr_ParamPlanta.CodigoPlanta, 1, Long(data),Integer(istr_mant.argumento[10])) THEN
			This.SetItem(row,"mfge_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			istr_mant.Argumento[3] = Data
		END IF

	CASE "mfge_fecmov"
		ld_Fecha	=	Date(Mid(String(date(data),'dd/mm/yyyy'),1,10))
		
		IF NOT iuo_FechaMovto.Valida_FechaMovto(ld_Fecha) THEN
			This.SetItem(row,"mfge_fecmov",Date(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF	

	CASE "espe_codigo"
		IF Not iuo_Especie.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
//		ELSEIF NOT iuo_Especie.ExisteCorrelativoLote(gstr_ParamPlanta.CodigoPlanta,Integer(Data),True,SQLCA) THEN
//			This.SetItem(row, ls_Columna, Integer(ls_Nula))
//			
//			RETURN 1
//		ELSE
			istr_mant.argumento[5]	=	data
			istr_mant.argumento[7]	=	'0'
			IF iuo_especie.kildec = 1 THEN
				ii_kildec = 2
			ELSE
				ii_kildec = 0
			END If
			
			idwc_Variedad.SetTransObject(SQLCA)
			idwc_Variedad.Retrieve(Integer(data))
			This.SetItem(row, ls_Columna, Integer(Data))
			This.AcceptText()
		END IF

	CASE "tran_codigo"
		IF Not iuo_Transport.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSE
			This.SetItem(row, ls_Columna, Integer(Data))
			This.AcceptText()
		END IF

	CASE "cami_patent"
		IF data <> "" AND Not iuo_Camion.Existe(1, Data, True, sqlca) THEN
			If Isnull(dw_2.Object.mfge_observ[row]) Then
				ls_pat =  'PATENTE N°: ' + data
			Else
				ls_pat = dw_2.Object.mfge_observ[row]+ ' PATENTE N°: ' + data
			end If
			This.SetItem(row,'mfge_observ',ls_pat)
			This.SetItem(row, ls_Columna, ls_Nula)
			
			RETURN 1
		ELSE
			This.Object.cami_patcar[row]	=	iuo_Camion.PateCarro
			This.Object.mfge_rutcho[row]	=	iuo_Camion.RutChofer
			is_rut = iuo_Camion.RutChofer
			This.Object.mfge_chofer[row]	=	iuo_Camion.Chofer
		END IF

	CASE "mfge_rutcho"
		
		is_rut = F_verrut(data, True)
		IF is_rut = "" THEN
			dw_2.SetItem(row, "mfge_rutcho", ls_Nula)
			dw_2.SetItem(row, "mfge_chofer", ls_Nula)
			RETURN 1
		ELSE
			IF NoExisteChofer(Data) THEN
				dw_2.SetItem(row, "mfge_rutcho", ls_Nula)
				dw_2.SetItem(row, "mfge_chofer", ls_Nula)
				RETURN 1
			ELSE	
				dw_2.SetItem(row, "mfge_chofer",is_chofer)
				dw_2.SetItem(row, "mfge_rutcho",is_rut)
				RETURN 1
			END IF
		END IF	

	CASE "refg_tkbent", "refg_tkbenc", "refg_tkbsal", "refg_tkbsac"
		IF Not This.uf_validate(row) THEN
			This.SetItem(row,ls_Columna,Dec(ls_Nula))
			RETURN 1
		ELSE
//			captura_Totales()
		END IF
		
	CASE "mfge_totbul"
		IF Integer(data) > 9999 OR Integer(data) < 0 THEN
			This.SetItem(row,"mfge_totbul",Integer(ls_Nula))
			RETURN 1
		END IF

	CASE "fecha_sal"
		ld_Fecha	=	Date(String(Mid(data,1,10),'dd/mm/yyyy'))
		
		IF NOT iuo_FechaMovto.Valida_FechaMovto(date(ld_Fecha)) OR &
			ld_Fecha < This.Object.mfge_fecmov[row] THEN
			MessageBox("Atención","Fecha de Salida no puede ser anterior a Fecha de Entrada")
			This.SetItem(row,"fecha_sal",SetNull(ld_Fecha))
			RETURN 1
		END IF
		
		
	CASE "refg_horasa"
		lt_Hora	=	Time(This.Object.fecha_sal[Row])
	
		IF Time(data) < This.Object.refg_horaen[row] THEN
			MessageBox("Atención","Hora de Salida no puede ser anterior a Hora de Entrada")
			This.SetItem(row,"refg_horasa",SetNull(lt_Hora))
			RETURN 1
		END IF
		
END CHOOSE

IF ls_Columna <> 'mfge_numero' THEN
	HabilitaIngreso(ls_Columna)

END IF
end event

event dw_2::buttonclicked;call super::buttonclicked;Long 				ll_Fila
String 			ls_nula
decimal			ld_peso
Str_busqueda	lstr_busq

SetNull(ls_nula)

CHOOSE CASE dwo.Name
	CASE "buscacamion"
		buscacamion()
		iuo_Camion.Existe(1, This.Object.cami_patent[row], True, sqlca)
	
END CHOOSE

IF dwo.Name <> 'mfge_numero' THEN
	HabilitaIngreso(dwo.Name)

END IF
end event

event dw_2::add_row;//
end event

event dw_2::clicked;//
end event

event dw_2::doubleclicked;//
end event

event dw_2::itemerror;Return 1
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutagranel_encabprod
integer x = 3099
integer y = 268
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutagranel_encabprod
integer x = 3099
integer y = 448
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutagranel_encabprod
integer x = 3099
integer y = 632
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutagranel_encabprod
integer x = 3099
integer y = 808
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutagranel_encabprod
integer x = 3099
integer y = 1084
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutagranel_encabprod
integer x = 3099
integer y = 1544
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutagranel_encabprod
boolean visible = false
integer x = 3099
integer y = 1548
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutagranel_encabprod
integer x = 3099
end type

type dw_3 from datawindow within w_maed_movtofrutagranel_encabprod
boolean visible = false
integer width = 87
integer height = 36
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_ctrol_printers"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event doubleclicked;String 	ls_impresoras, ls_variable, ls_caracter, ls_ImpresoraActual
Integer	li_fila, li_posi, li_columna, li_row

ls_impresoras 		= 	PrintGetPrinters ( )
ls_ImpresoraActual=	PrintGetPrinter ( )

DO WHILE Len(ls_impresoras) > 0
	li_columna		=	1
	li_posi			=	Pos(ls_impresoras, '~n')
	
	IF li_posi = 0 THEN
		ls_impresoras	=	ls_impresoras + '~n'
		li_posi	 		=	Pos(ls_impresoras, '~n')
	END IF
	FOR li_fila = 1 TO li_posi
		ls_caracter	=	Mid(ls_impresoras, li_fila, 1)
		IF ls_caracter <> '~t' AND ls_caracter <> '~n' THEN
			ls_variable = 	ls_variable + ls_caracter
		ELSE
			CHOOSE CASE li_columna
				CASE 1
					li_row	=	dw_3.InsertRow(0)
					dw_3.Object.impresora[li_row] = ls_variable
					
				CASE 2
					dw_3.Object.drivers[li_row] 	= ls_variable
					
				CASE 3
					dw_3.Object.puerto[li_row] 	= ls_variable
					
			END CHOOSE
			li_columna 		++
			ls_variable 	= 	''
		END IF
	NEXT
	ls_impresoras	=	Right(ls_impresoras, (Len(ls_impresoras) - li_posi))
LOOP

/*
luego de cargar los nombres y parametros de las impresoras,
se debe armar un string con impresora + '~t' + drivers + '~t' + puerto, 
asignarlo como impresora predeterminada, mandar la impresion automatica
y devolver la impresora predeterminada con el siguiente comando.
*/


end event

type dw_4 from datawindow within w_maed_movtofrutagranel_encabprod
boolean visible = false
integer width = 311
integer height = 244
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_adhesivo_encabezado"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

