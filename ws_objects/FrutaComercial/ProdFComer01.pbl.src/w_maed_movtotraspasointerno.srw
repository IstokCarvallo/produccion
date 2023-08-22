$PBExportHeader$w_maed_movtotraspasointerno.srw
$PBExportComments$Recepción - Despacho de Fruta Comercial.
forward
global type w_maed_movtotraspasointerno from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_movtotraspasointerno
end type
type tp_1 from userobject within tab_1
end type
type dw_detalle from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_detalle dw_detalle
end type
type tp_2 from userobject within tab_1
end type
type dw_envases from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_envases dw_envases
end type
type tab_1 from tab within w_maed_movtotraspasointerno
tp_1 tp_1
tp_2 tp_2
end type
type cb_capturadatos from commandbutton within w_maed_movtotraspasointerno
end type
type dw_6 from datawindow within w_maed_movtotraspasointerno
end type
type dw_5 from datawindow within w_maed_movtotraspasointerno
end type
end forward

global type w_maed_movtotraspasointerno from w_mant_encab_deta_csd
integer width = 3671
integer height = 2144
string title = "TRASPASOS INTERNOS"
string menuname = ""
event ue_imprimir ( )
event ue_carga_detalle pbm_custom27
tab_1 tab_1
cb_capturadatos cb_capturadatos
dw_6 dw_6
dw_5 dw_5
end type
global w_maed_movtotraspasointerno w_maed_movtotraspasointerno

type variables
w_mant_deta_movtotraspasointerno		iw_mantencion_1
w_mant_deta_movtoenvadeta			   iw_mantencion_2

uo_plantadesp			iuo_PltaDestino,iuo_planta
uo_Productores			iuo_Productor
uo_transportista		iuo_Transport
uo_camiones				iuo_Camion
uo_tipodoctoplanta	iuo_TipoDocto
uo_clienprove			iuo_ClienProve
uo_lotesfrutacomer	iuo_LotesFrutaComer
uo_tipomovtofruta		iuo_TipoMovtoFruta
uo_tipomovtofruta		iuo_TipoMovtoEnva
Boolean					ib_Modifica, ib_AutoCommit, ib_Predefinido
DataWindowChild   	idwc_PltaDest, idwc_Transp, idwc_Camion, idwc_TipoMovto
DataWindow				dw_3, dw_4
str_variedad			istr_variedad
str_categoria			istr_categoria

Integer					ii_TipoMovto, ii_Movto
String					is_rut, is_columna,is_rutcliente,is_archivo
end variables

forward prototypes
public function boolean existemovimiento (integer ai_planta, integer ai_tipomovto, long al_numero)
public subroutine modificaencab (integer is_movto)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitaingreso (string as_columna)
public subroutine buscacamion ()
end prototypes

event ue_imprimir();//SetPointer(HourGlass!)
//
//Long		fila
//str_info	lstr_info
//
//lstr_info.titulo	= "MAESTRO DE GRUPOS DE VARIEDADES"
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
//vinf.dw_1.DataObject = "dw_info_grupovariedad"
//
//vinf.dw_1.SetTransObject(sqlca)
//
////fila = vinf.dw_1.Retrieve(istr_mant.argumento[1])
//fila = vinf.dw_1.Retrieve()
//IF fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF fila =0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//
//SetPointer(Arrow!)
end event

event ue_carga_detalle;w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

Long		ll_Nuevo, ll_NumeroLote, ll_TotalBultos,ll_Productor
String	ls_datos, ls_Patente, ls_PatCarro, ls_Calida, ls_Cliente, ls_RutChofer, ls_Chofer, &
			ls_GrupoCal, ls_TipoFrio, ls_Calibre
Integer	li_retorno = 2, li_archivo, li_Tipo, li_Planta, li_PlantaCoorde, li_Transporte, &
			li_Camion, li_Camara, li_Pltcod, li_EspCod, li_SecLote, &
			li_TipoEnvase, li_Envase, li_ConEnv, li_Cantidad, li_Categoria, li_Servicio, &
			li_DiasGr, li_Turno, li_Variedad, li_PeriodoFrio, li_Secuencia, li_Null
Dec{3}	ld_Kilos, ld_PesoNeto, ld_KilProm
Dec{2}	ld_Bultos
Date		ld_FechaMovto, ld_FechaLote

SetNull(li_Null)

li_archivo	= FileOpen(is_archivo)

IF li_archivo < 0 THEN
	li_retorno = MessageBox("Error","Archivo no pudo ser abierto.", Exclamation!,RetryCancel!)
	Message.DoubleParm = li_retorno
	RETURN 1
ELSE
	SetPointer(HourGlass!)

	dw_3.SetRedraw(False)
	dw_4.SetRedraw(False)

	DO WHILE FileRead(li_archivo, ls_datos) >= 0
		li_Tipo				=	Integer(Mid(ls_datos,1,1))

		IF li_Tipo	=	1	THEN
			li_Planta			=	Integer(Mid(ls_datos,2,4))
			ld_FechaMovto		=	Date(Mid(ls_datos,9,2) + "/" + Mid(ls_datos,11,2) + "/" + &
										Mid(ls_datos,13,4))

			ll_Productor		=	long(Mid(ls_datos,19,4))
			IF ll_Productor = 0 THEN
				ll_Productor = li_Null
			END IF

			li_PlantaCoorde	=	Integer(Mid(ls_datos,23,4))
			li_Transporte		=	Integer(Mid(ls_datos,27,4))
			ls_Cliente			=	Trim(Mid(ls_datos,31,10))
			li_Camion			=	Integer(Mid(ls_datos,41,1))
			ls_Patente			=	Trim(Mid(ls_datos,42,10))
			ls_PatCarro			=	Trim(Mid(ls_datos,52,10))
			ls_RutChofer		=	Trim(Mid(ls_datos,70,10))
			ls_Chofer			=	Trim(Mid(ls_datos,80,40))
			ll_TotalBultos		=	Long(Mid(ls_datos,120,4))
			ld_PesoNeto			=	Dec(Mid(ls_datos,124,12))

			// Encabezado Movimento Fruta Comercial
			dw_2.SetItem(1, "plde_codigo", li_Planta)
			dw_2.SetItem(1, "tpmv_codigo", Integer(istr_Mant.Argumento[2]))
			dw_2.SetItem(1, "prod_codigo", ll_Productor)
			dw_2.SetItem(1, "plde_coorde", li_PlantaCoorde)
			dw_2.SetItem(1, "tran_codigo", li_Transporte)
			dw_2.SetItem(1, "clpr_rut", ls_Cliente)
			dw_2.SetItem(1, "cami_clasifi", li_Camion)
			dw_2.SetItem(1, "cami_patent", ls_Patente)
			dw_2.SetItem(1, "cami_patcar", ls_PatCarro)
			dw_2.SetItem(1, "mfco_rutcho", ls_RutChofer)
			dw_2.SetItem(1, "mfco_chofer", ls_Chofer)
			dw_2.SetItem(1, "mfco_fecmov", ld_FechaMovto)
			dw_2.SetItem(1, "mfco_totbul", ll_TotalBultos)
			dw_2.SetItem(1, "mfco_tpneto", ld_PesoNeto)

		ELSEIF	li_Tipo	=	2	THEN
			li_PlantaCoorde	=	Integer(Mid(ls_datos,2,4))
			li_Camara			=	Integer(Mid(ls_datos,6,4))
			li_PltCod			=	Integer(Mid(ls_datos,10,4))
			li_EspCod			=	Integer(Mid(ls_datos,14,2))
			ll_NumeroLote		=	Long(Mid(ls_datos,16,8))
			li_SecLote			=	Integer(Mid(ls_datos,24,2))
			ld_Bultos			=	Dec(Mid(ls_datos,26,9))
			ld_Kilos				=	Dec(Mid(ls_datos,35,12))
			ld_KilProm			=	Dec(Mid(ls_datos,47,12))

			ll_Nuevo				=	dw_3.InsertRow(0)

			// Detalle de Movimiento Fruta Comercial
			dw_3.SetItem(ll_Nuevo, "plde_coorde", li_PlantaCoorde)
			dw_3.SetItem(ll_Nuevo, "cama_codigo", li_Camara)
			dw_3.SetItem(ll_Nuevo, "lofc_pltcod", li_PltCod)
			dw_3.SetItem(ll_Nuevo, "lofc_espcod", li_EspCod)
			dw_3.SetItem(ll_Nuevo, "lofc_lotefc", ll_NumeroLote)
			dw_3.SetItem(ll_Nuevo, "lfcd_secuen", li_SecLote)
			dw_3.SetItem(ll_Nuevo, "mfcd_bulent", ld_Bultos)
			dw_3.SetItem(ll_Nuevo, "mfcd_kgnent", ld_Kilos)
			dw_3.SetItem(ll_Nuevo, "mfcd_kilrom", ld_KilProm)

		ELSEIF	li_Tipo	=	3	THEN
			ll_Productor		=	long(Mid(ls_datos,2,4))
			li_Categoria		=	Integer(Mid(ls_datos,6,3))
			li_Servicio			=	Integer(Mid(ls_datos,9,2))
			li_DiasGr			=	Integer(Mid(ls_datos,11,3))
			ls_GrupoCal			=	Trim(Mid(ls_datos,14,3))
			ll_TotalBultos		=	Long(Mid(ls_datos,17,4))
			ld_Kilos				=	Dec(Mid(ls_datos,21,12))

			IF NOT iuo_LotesFrutaComer.ExisteEncab(Integer(istr_Mant.Argumento[1]), &
												li_EspCod, ll_NumeroLote, False, SqlCa) THEN

				ll_Nuevo				=	dw_5.InsertRow(0)

				// Encabezado de Lote Fruta Comercial
				dw_5.SetItem(ll_Nuevo, "lofc_espcod", li_EspCod)
				dw_5.SetItem(ll_Nuevo, "lofc_lotefc", ll_NumeroLote)
				dw_5.SetItem(ll_Nuevo, "prod_codigo", ll_Productor)
				dw_5.SetItem(ll_Nuevo, "cate_codigo", li_Categoria)
				dw_5.SetItem(ll_Nuevo, "sepl_codigo", li_Servicio)
				dw_5.SetItem(ll_Nuevo, "lofc_diagra", li_DiasGr)
				dw_5.SetItem(ll_Nuevo, "lofc_grucal", ls_GrupoCal)
				dw_5.SetItem(ll_Nuevo, "lofc_totbul", ll_TotalBultos)
				dw_5.SetItem(ll_Nuevo, "lofc_totkil", ld_Kilos)
			END IF

		ELSEIF	li_Tipo	=	4	THEN
			li_Secuencia		=	Integer(Mid(ls_datos,2,2))
			li_Turno				=	Integer(Mid(ls_datos,4,1))
			ld_FechaLote		=	Date(Mid(ls_datos,5,2) + "/" + Mid(ls_datos,7,2) + "/" + &
										Mid(ls_datos,9,4))

			ll_Productor		=	long(Mid(ls_datos,13,4))
			li_Variedad			=	Integer(Mid(ls_datos,17,4))
			ls_TipoFrio			=	Trim(Mid(ls_datos,21,2))
			li_PeriodoFrio		=	Integer(Mid(ls_datos,23,2))
			li_TipoEnvase		=	Integer(Mid(ls_datos,25,1))
			li_Envase			=	Integer(Mid(ls_datos,26,3))
			ls_Calibre			=	Trim(Mid(ls_datos,29,4))
			ls_GrupoCal			=	Trim(Mid(ls_datos,33,4))
			ld_Bultos			=	Dec(Mid(ls_datos,37,9))
			ld_PesoNeto			=	Dec(Mid(ls_datos,46,12))
			ld_KilProm			=	Dec(Mid(ls_datos,58,12))

			IF NOT iuo_LotesFrutaComer.ExisteEncab(Integer(istr_Mant.Argumento[1]), &
												li_EspCod, ll_NumeroLote, False, SqlCa) THEN

				ll_Nuevo				=	dw_6.InsertRow(0)

				// Detalle de Lote Fruta Comercial
				dw_6.SetItem(ll_Nuevo, "lofc_espcod", li_EspCod)
				dw_6.SetItem(ll_Nuevo, "lofc_lotefc", ll_NumeroLote)
				dw_6.SetItem(ll_Nuevo, "lfcd_secuen", li_Secuencia)
				dw_6.SetItem(ll_Nuevo, "lfcd_nturno", li_Turno)
				dw_6.SetItem(ll_Nuevo, "lfcd_fecham", ld_FechaLote)
				dw_6.SetItem(ll_Nuevo, "prod_codigo", ll_Productor)
				dw_6.SetItem(ll_Nuevo, "vari_codigo", li_Variedad)
				dw_6.SetItem(ll_Nuevo, "frio_tipofr", ls_TipoFrio)
				dw_6.SetItem(ll_Nuevo, "pefr_codigo", li_PeriodoFrio)
				dw_6.SetItem(ll_Nuevo, "enva_tipoen", li_TipoEnvase)
				dw_6.SetItem(ll_Nuevo, "enva_codigo", li_Envase)
				dw_6.SetItem(ll_Nuevo, "refe_calibr", ls_Calibre)
				dw_6.SetItem(ll_Nuevo, "refe_gcalib", ls_GrupoCal)
				dw_6.SetItem(ll_Nuevo, "lfcd_bultos", ld_Bultos)
				dw_6.SetItem(ll_Nuevo, "lfcd_kilnet", ld_PesoNeto)
				dw_6.SetItem(ll_Nuevo, "lfcd_kilpro", ld_KilProm)
			END IF

		ELSEIF	li_Tipo	=	5	THEN
			li_TipoEnvase		=	Integer(Mid(ls_datos,2,1))
			li_Envase			=	Integer(Mid(ls_datos,3,3))
			li_ConEnv			=	Integer(Mid(ls_datos,6,1))
			ls_Calida			=	Trim(Mid(ls_datos,7,4))
			li_Cantidad			=	Integer(Mid(ls_datos,12,4))
			ld_PesoNeto			=	Dec(Mid(ls_datos,16,8))

			ll_Nuevo				=	dw_4.InsertRow(0)

			// Detalle de Movimiento Envase
			dw_4.SetItem(ll_Nuevo, "enva_tipoen", li_TipoEnvase)
			dw_4.SetItem(ll_Nuevo, "enva_codigo", li_Envase)
			dw_4.SetItem(ll_Nuevo, "fgme_conenv", li_ConEnv)
			dw_4.SetItem(ll_Nuevo, "cale_calida", ls_Calida)
			dw_4.SetItem(ll_Nuevo, "fgme_sentid", Integer(istr_Mant.Argumento[4]))
			dw_4.SetItem(ll_Nuevo, "fgme_cantid", li_Cantidad)
			dw_4.SetItem(ll_Nuevo, "fgme_pesone", ld_PesoNeto)
		END IF
	LOOP
	dw_3.SetRedraw(True)
	dw_4.SetRedraw(True)
END IF

FileClose(li_archivo)

Message.DoubleParm = li_retorno

RETURN 0

SetPointer(Arrow!)
end event

public function boolean existemovimiento (integer ai_planta, integer ai_tipomovto, long al_numero);Long		ll_Numero
Boolean	lb_Retorno = True

SELECT	mfco_numero
	INTO	:ll_Numero
	FROM	dba.spro_movtofrutacomenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfco_numero	=	:al_Numero ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento Fruta Comerial")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[3]	=	String(al_Numero)
	This.TriggerEvent("ue_recuperadatos")
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine modificaencab (integer is_movto);
IF Integer(istr_Mant.Argumento[4]) = 1 THEN
	
	IF is_Movto	=	2 THEN
		
		This.Title	=	"Recepción Otras Plantas"
		cb_capturadatos.Visible	=	True
		cb_capturadatos.Enabled	=	True
		
		dw_2.Object.mfco_horasa.Visible	=	False
		dw_2.Modify("planta_t.Text = 'Planta Origen'")

		//No Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	1
		dw_2.Object.moti_codigo.BackGround.Color	= rgb(166,180,210)
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.prod_codigo_t.visible			=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",1)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.BackGround.Color	= rgb(166,180,210)
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
		
		
	END IF
	
	
ELSE
	
	IF is_Movto	=	22 THEN
		
		This.Title	=	"Despacho Otras Plantas"		
		dw_2.Object.mfco_horaen.Visible	=	False
		dw_2.Modify("planta_t.Text = 'Planta Destino'")

		//Si Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	0
		dw_2.Object.moti_codigo.BackGround.Color	= RGB(255,255,255)
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.prod_codigo_t.visible			=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",2)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.BackGround.Color	= rgb(166,180,210)
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
		
	END IF

	IF is_Movto	=	23 THEN
		This.Title	=	"Devolución a Productores"
		
		//Si Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	0
		dw_2.Object.moti_codigo.BackGround.Color	= RGB(255,255,255)
		
		//Si Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	1
		dw_2.Object.prod_codigo.Protect				=	0
		dw_2.Object.prod_codigo.BackGround.Color	= RGB(255,255,255)
		dw_2.Object.prod_codigo_t.visible			=	1
		
		//No Solicita Planta Destino
		dw_2.Object.plde_coorde.visible				=	0
		dw_2.Object.plde_coorde.Protect				=	1
		dw_2.Object.plde_coorde.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.planta_t.visible					=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",2)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.BackGround.Color	= rgb(166,180,210)
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
		
	END IF
	
	IF is_Movto	=	34 THEN
		
		This.Title	=	"Mermas"
		
		//Si Solicita Motivo 
		dw_2.Object.moti_codigo.Protect				=	0
		dw_2.Object.moti_codigo.BackGround.Color	= RGB(255,255,255)
		
		//No Solicita Productor 
		dw_2.Object.prod_codigo.visible				=	0
		dw_2.Object.prod_codigo.Protect				=	1
		dw_2.Object.prod_codigo.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.prod_codigo_t.visible			=	0
		
		//No Solicita Planta Destino
		dw_2.Object.plde_coorde.visible				=	0
		dw_2.Object.plde_coorde.Protect				=	1
		dw_2.Object.plde_coorde.BackGround.Color	= rgb(166,180,210)
		dw_2.Object.planta_t.visible					=	0
				
		//Doc. Relacionado Guía de Recepción
		dw_2.Setitem(1,"mfco_tipdoc",2)
		dw_2.Object.mfco_tipdoc.Protect				=	1
		dw_2.Object.mfco_tipdoc.BackGround.Color	= rgb(166,180,210)
		
		//Invisible Doc. Relacionado
		dw_2.Object.mfco_docrel.visible				=	0
		dw_2.Object.mfco_docrel_t.visible			=	0
		
	END IF
END IF
end subroutine

public subroutine habilitaencab (boolean habilita);
IF Habilita THEN
	
	dw_2.Object.mfco_numero.Protect				   =	0
	dw_2.Object.mfco_numero.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfco_fecmov.Protect					=	0
	dw_2.Object.mfco_fecmov.BackGround.Color		=	RGB(255,255,255)
   
	IF integer(istr_mant.argumento[4]) = 1 THEN
      dw_2.Object.mfco_horaen.Protect				=	0
  		dw_2.Object.mfco_horaen.BackGround.Color	=	RGB(255,255,255)
   ELSE		  
		dw_2.Object.mfco_horasa.Protect				=	0
		dw_2.Object.mfco_horasa.BackGround.Color	=	RGB(255,255,255)
	END IF	

   dw_2.Object.plde_codigo.Protect				=	0
	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.mfco_tipdoc.Protect				=	0
	dw_2.Object.mfco_tipdoc.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.plde_coorde.Protect				=	0
	dw_2.Object.plde_coorde.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.moti_codigo.Protect				=	0
	dw_2.Object.moti_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.tran_codigo.Protect				=	0
	dw_2.Object.tran_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfco_guisii.Protect				=	0
	dw_2.Object.mfco_guisii.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.cami_patent.Protect				=	0
	dw_2.Object.cami_patent.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Protect				=	0
	dw_2.Object.cami_patcar.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfco_rutcho.Protect				=	0
	dw_2.Object.mfco_rutcho.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfco_chofer.Protect				=	0
	dw_2.Object.mfco_chofer.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.b_camion.visible					=	1
	
ELSE
	
	dw_2.Object.mfco_numero.Protect				=	1
	dw_2.Object.mfco_numero.BackGround.Color	=	rgb(166,180,210)
	dw_2.Object.mfco_fecmov.Protect				=	1
	dw_2.Object.mfco_fecmov.BackGround.Color	=	rgb(166,180,210)
	
	IF integer(istr_mant.argumento[4]) = 1 THEN
      dw_2.Object.mfco_horaen.Protect				=	1
  		dw_2.Object.mfco_horaen.BackGround.Color	=	rgb(166,180,210)	
   ELSE		  
		dw_2.Object.mfco_horasa.Protect				=	1
		dw_2.Object.mfco_horasa.BackGround.Color	=	rgb(166,180,210)	
	END IF	
	
	dw_2.Object.plde_codigo.Protect				=	1
	dw_2.Object.plde_codigo.BackGround.Color	=	rgb(166,180,210)	
	dw_2.Object.mfco_tipdoc.Protect				=	1
	dw_2.Object.mfco_tipdoc.BackGround.Color	=	rgb(166,180,210)		
	dw_2.Object.plde_coorde.Protect				=	1
	dw_2.Object.plde_coorde.BackGround.Color	=	rgb(166,180,210)	
	dw_2.Object.moti_codigo.Protect				=	1
	dw_2.Object.moti_codigo.BackGround.Color	=	rgb(166,180,210)
	dw_2.Object.mfco_guisii.Protect				=	1
	dw_2.Object.mfco_guisii.BackGround.Color	=	rgb(166,180,210)	
	dw_2.Object.cami_patent.Protect				=	1
	dw_2.Object.cami_patent.BackGround.Color	=	rgb(166,180,210)
	dw_2.Object.cami_patcar.Protect				=	1
	dw_2.Object.cami_patcar.BackGround.Color	=	rgb(166,180,210)

	dw_2.Object.b_camion.visible					=	0
	
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"mfco_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"mfco_comput",gstr_us.computador)
		dw_2.SetItem(1,"mfco_horacr",F_FechaHora())
	ELSEIF dw_2.GetItemStatus(1, 0, Primary!) = DataModified! THEN
		dw_2.SetItem(1,"mfco_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"mfco_commod",gstr_us.computador)
		dw_2.SetItem(1,"mfco_horact",F_FechaHora())
	END IF
END IF

IF Borrando THEN
	IF dw_4.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				IF dw_2.Update(True, False) = 1 THEN
					Commit;
			
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
					ELSE
						lb_Retorno	=	True
			
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						dw_3.ResetUpdate()
						dw_4.ResetUpdate()
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

		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_5.Update(True, False) = 1 THEN
			IF dw_6.Update(True, False) = 1 THEN
				IF dw_3.Update(True, False) = 1 THEN
					IF dw_1.Update(True, False) = 1 THEN
						IF dw_4.Update(True, False) = 1 THEN
							Commit;
			
							IF sqlca.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlca, This.Title)
							ELSE
								lb_Retorno	=	True
			
								dw_1.ResetUpdate()
								dw_2.ResetUpdate()
								dw_3.ResetUpdate()
								dw_4.ResetUpdate()
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
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date	   ldt_Fecha

IF as_Columna <> "mfco_fecmov" AND &
	(dw_2.Object.mfco_fecmov[1] = ldt_Fecha OR IsNull(dw_2.Object.mfco_fecmov[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "plde_coorde" AND &
	(dw_2.Object.plde_coorde[1] = 0 OR IsNull(dw_2.Object.plde_coorde[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "tran_codigo" AND &
	IsNull(dw_2.Object.tran_codigo[1]) THEN
	lb_Estado = False
END IF
IF as_Columna <> "mfco_guisii" AND &
	(dw_2.Object.mfco_guisii[1] = 0 OR IsNull(dw_2.Object.mfco_guisii[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "moti_codigo" AND &
	(dw_2.Object.moti_codigo[1] = 0 OR IsNull(dw_2.Object.moti_codigo[1])) THEN
	lb_Estado = False
END IF
	
tab_1.tp_1.Enabled	=	lb_Estado
tab_1.tp_2.Enabled	=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado

end subroutine

public subroutine buscacamion ();Str_busqueda	lstr_busq
Integer			li_ClasifCamion

IF Integer(istr_Mant.Argumento[2]) = 2 OR Integer(istr_Mant.Argumento[2]) = 22 THEN
	lstr_busq.argum[1] = '2'
ELSE
	lstr_busq.argum[1] = '1'
END IF

OpenWithParm(w_busc_camiones, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("cami_patent")
	dw_2.SetFocus()
ELSE
	dw_2.Object.cami_clasifi[1]	=	Integer(lstr_busq.argum[1])
	dw_2.Object.cami_patent[1]		=	lstr_busq.argum[2]
	dw_2.Object.cami_patcar[1]		=	lstr_busq.argum[6]
	dw_2.Object.mfco_rutcho[1]		=	lstr_busq.argum[5]
	dw_2.Object.mfco_chofer[1]		=	lstr_busq.argum[4]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Tipo de Movimiento
istr_Mant.Argumento[3]	=	Número de Movimiento
istr_Mant.Argumento[4]	=	Sentido del Movimiento
istr_Mant.Argumento[5]	=	Código Planta Dest
*/

DataWindowChild	ldwc_Camara, ldwc_TipoEnvase, ldwc_PltaLote, ldwc_Especie

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal
This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

dw_3	=	tab_1.tp_1.dw_detalle
dw_4	=	tab_1.tp_2.dw_envases

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[3]	=	""

istr_Mant.Argumento[7]	=	"0"

istr_Mant.Argumento[2]	=	'10'
istr_Mant.Argumento[4]	=	'1'

pb_nuevo.PostEvent(Clicked!)


dw_2.GetChild("plde_coorde", idwc_PltaDest)
idwc_PltaDest.SetTransObject(sqlca)

IF idwc_PltaDest.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_PltaDest.InsertRow(0)
ELSE
	idwc_PltaDest.SetFilter("plde_codigo <> " + &
									String(gstr_ParamPlanta.CodigoPlanta))
	idwc_PltaDest.Filter()
	idwc_PltaDest.SetSort("plde_nombre A")
	idwc_PltaDest.Sort()
END IF

dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

IF idwc_Transp.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Transportístas")
	idwc_Transp.InsertRow(0)
ELSE
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()
END IF

dw_3.GetChild("cama_codigo", ldwc_Camara)
ldwc_Camara.SetTransObject(sqlca)

IF ldwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	ldwc_Camara.InsertRow(0)
ELSE
	ldwc_Camara.SetSort("cama_nombre A")
	ldwc_Camara.Sort()
END IF

dw_3.GetChild("lote_pltcod", ldwc_PltaLote)
ldwc_PltaLote.SetTransObject(sqlca)
ldwc_PltaLote.Retrieve()

dw_3.GetChild("lote_espcod", ldwc_Especie)
ldwc_Especie.SetTransObject(sqlca)
ldwc_Especie.Retrieve()

dw_4.GetChild("enva_tipoen", ldwc_TipoEnvase)
ldwc_TipoEnvase.SetTransObject(sqlca)

IF ldwc_TipoEnvase.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Envases ")
	ldwc_TipoEnvase.InsertRow(0)
ELSE
	ldwc_TipoEnvase.SetSort("tien_nombre A")
	ldwc_TipoEnvase.Sort()
END IF

dw_4.GetChild("cama_codigo", ldwc_Camara)
ldwc_Camara.SetTransObject(sqlca)

IF ldwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	ldwc_Camara.InsertRow(0)
ELSE
	ldwc_Camara.SetSort("cama_nombre A")
	ldwc_Camara.Sort()
END IF

dw_1.SetTransObject(sqlca) // Encabezado Movimiento de Envase
dw_2.SetTransObject(sqlca) // Encabezado Movimiento de Fruta Comercial
dw_3.SetTransObject(sqlca) // Detalle de Movimiento de Fruta Comercial
dw_4.SetTransObject(sqlca) // Detalle de Movimiento de Envase
dw_5.SetTransObject(sqlca) // Encabezado Lotes Fruta Comercial
dw_6.SetTransObject(sqlca) // Detalle de Lotes Fruta Comercial

dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.SetRowFocusIndicator(Hand!)
dw_3.Modify("DataWindow.Footer.Height = 88")

dw_4.Modify("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_4.Modify("DataWindow.Footer.Height = 88")

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

iuo_PltaDestino		=	Create uo_plantadesp
iuo_Planta				=	Create uo_plantadesp
iuo_Productor			=	Create uo_Productores
iuo_Transport			=	Create uo_transportista
iuo_Camion				=	Create uo_camiones
iuo_TipoDocto			=	Create uo_tipodoctoplanta
iuo_ClienProve			=	Create uo_clienprove
iuo_LotesFrutaComer	=	Create uo_lotesfrutacomer
iuo_tipomovtofruta	=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva		=	Create uo_tipomovtofruta		

end event

event ue_borra_detalle();call super::ue_borra_detalle;SetPointer(HourGlass!)

ib_Borrar				=	True
Message.DoubleParm	=	0
istr_mant.Borra		=	True
istr_mant.Agrega		=	False

w_main.SetMicroHelp("Validando la eliminación de detalle...")

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF tab_1.SelectedTab = 1 THEN
	istr_Mant.dw	=	dw_3
	
	OpenWithParm(iw_mantencion_1, istr_Mant)
	
	istr_Mant	=	Message.PowerObjectParm
	
	IF istr_mant.Respuesta = 1 THEN
		IF dw_3.DeleteRow(0) = 1 THEN
			ib_Borrar	=	False
			
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_Borrar	=	False
			
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_3.RowCount() = 0 THEN 
			HabilitaEncab(True)
			
			pb_eli_det.Enabled			=	False
			dw_2.Object.mfco_totbul[1]	=	0
			dw_2.Object.mfco_tpneto[1]	=	0
		ELSE
			dw_2.Object.mfco_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
			dw_2.Object.mfco_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
		END IF
	END IF
ELSE
	istr_Mant.dw	=	dw_4
	
	OpenWithParm(iw_mantencion_2, istr_Mant)
	
	istr_Mant	=	Message.PowerObjectParm
	
	IF istr_mant.Respuesta = 1 THEN
		IF dw_4.DeleteRow(0) = 1 THEN
			ib_Borrar	=	False
			
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_Borrar	=	False
			
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_4.RowCount() = 0 THEN 
			HabilitaEncab(True)
			pb_eli_det.Enabled	=	False
		END IF
	END IF
END IF

istr_mant.Borra	 = False
end event

event ue_nuevo_detalle();call super::ue_nuevo_detalle;istr_mant.Borra	=	False
istr_mant.Agrega	=	True
Istr_Mant.Argumento[5] = String(dw_2.Object.plde_coorde[1])  
habilitaencab( False)

IF tab_1.SelectedTab = 1 THEN
	istr_mant.dw	=	dw_3
	
	OpenWithParm(iw_mantencion_1, istr_mant)

	IF dw_3.RowCount() > 0 THEN
		dw_2.Object.mfco_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
		dw_2.Object.mfco_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
		pb_eli_det.Enabled			=	True
	ELSE
		dw_2.Object.mfco_totbul[1]	=	0
		dw_2.Object.mfco_tpneto[1]	=	0
		pb_eli_det.Enabled			=	False
	END IF
ELSE
	istr_mant.dw	=	dw_4
	
	OpenWithParm(iw_mantencion_2, istr_mant)

	IF dw_4.RowCount() > 0 THEN
		pb_eli_det.Enabled			=	True
	ELSE
		pb_eli_det.Enabled			=	False
	END IF
END IF

IF dw_3.RowCount() > 0 AND dw_4.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_3.RowCount() > 0 AND dw_4.RowCount() > 0 AND Not pb_eliminar.Enabled THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
END IF

IF tab_1.SelectedTab = 1 THEN
	dw_3.SetRow(il_Fila)
	dw_3.SelectRow(il_Fila, True)
ELSE
	dw_4.SetRow(il_Fila)
	dw_4.SelectRow(il_Fila, True)
END IF
end event

event ue_recuperadatos();Long		ll_fila_d, ll_fila_e, respuesta, ll_Numero, ll_Numenva
Integer	li_TipoMovtoEnv, li_TipoMovto, li_planta

li_Planta		=	Integer(istr_Mant.Argumento[1])
li_TipoMovto	=	Integer(istr_Mant.Argumento[2])
ll_Numero		=	Long(istr_Mant.Argumento[3])


li_TipoMovtoEnv =	65

SELECT	meen_numero
	INTO	:ll_Numenva
	FROM	dba.spro_movtoenvaenca
	WHERE	plde_codigo	=	:li_Planta
  	  AND	tpmv_codigo	=	:li_TipoMovtoEnv
	  AND tpmv_codrec	=	:li_TipoMovto
	  AND mfge_numero =	:ll_Numero;

DO
	dw_2.SetRedraw(False)

	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
										  Integer(istr_Mant.Argumento[2]), &
										  Long(istr_Mant.Argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			IF dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[3])) = -1 OR &
				dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  li_TipoMovtoEnv, &
								  ll_Numenva) = -1 OR &
				dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  li_TipoMovto, &
								  ll_Numero, &
								  Integer(istr_Mant.Argumento[4])) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", &
												"No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True

				tab_1.tp_1.Enabled	=	True
				tab_1.tp_2.Enabled	=	True

				HabilitaEncab(False)
				
				pb_eli_det.Enabled	=	True
				
				dw_1.SetRow(1)
				dw_1.SelectRow(1,True)
				dw_1.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_movtotraspasointerno.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.cb_capturadatos=create cb_capturadatos
this.dw_6=create dw_6
this.dw_5=create dw_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.cb_capturadatos
this.Control[iCurrent+3]=this.dw_6
this.Control[iCurrent+4]=this.dw_5
end on

on w_maed_movtotraspasointerno.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.cb_capturadatos)
destroy(this.dw_6)
destroy(this.dw_5)
end on

event ue_modifica_detalle();istr_mant.agrega	=	False
istr_mant.borra	=	False

IF tab_1.SelectedTab = 1 THEN
	IF dw_3.RowCount() > 0 THEN
		istr_mant.dw		=	dw_3

		OpenWithParm(iw_mantencion_1, istr_mant)

		dw_2.Object.mfco_totbul[1]	=	Round(dw_3.Object.total_bultos[1], 0)
		dw_2.Object.mfco_tpneto[1]	=	Round(dw_3.Object.total_kilos[1], 3)
	ELSE
		dw_2.Object.mfco_totbul[1]	=	0
		dw_2.Object.mfco_tpneto[1]	=	0
	END IF
ELSE
	IF dw_4.RowCount() > 0 THEN
		istr_mant.dw		=	dw_4

		OpenWithParm(iw_mantencion_2, istr_mant)
	END IF
END IF
end event

event ue_nuevo();call super::ue_nuevo;is_rut 			= ""
is_rutcliente	= ""

Call Super::ue_nuevo

dw_3.Reset()
dw_4.Reset()

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
dw_2.Object.mfco_fecmov[1]	=	Date(Today())

modificaencab(integer(istr_mant.Argumento[2]))

IF istr_Mant.Argumento[4] = "1" THEN
	dw_2.Object.mfco_horaen[1]	=	Time(Now())
ELSE
	dw_2.Object.mfco_horasa[1]	=	Time(Now())
END IF
end event

event ue_antesguardar();Long		ll_Fila, ll_Numero, ll_Numenva
Integer	li_Secuencia, li_Planta, li_TipoMovto, li_TipoMovtoEnv

//ib_AutoCommit		=	sqlca.AutoCommit
//sqlca.AutoCommit	=	False
li_Planta			=	dw_2.Object.plde_codigo[1]
li_TipoMovto		=	dw_2.Object.tpmv_codigo[1]
ll_Numero			=  dw_2.Object.mfco_numero[1]	

SELECT	meen_numero
	INTO	:ll_Numenva
	FROM	dba.spro_movtoenvaenca
	WHERE	plde_codigo	=	:li_Planta
  	  AND	tpmv_codigo	=	65
	  AND tpmv_codrec	=	:li_TipoMovto
	  AND mfge_numero =	:ll_Numero;
	  
li_TipoMovtoEnv =	65

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN

	iuo_TipoMovtoFruta.bloqueacorrel()
	
	ll_Numenva = iuo_TipoMovtoEnva.ultimocorrel(li_Planta,li_TipoMovtoEnv) 
	
	IF ll_Numenva = 0 THEN
		RETURN
	END IF
	
	iuo_TipoMovtoEnva.bloqueacorrel()
	ll_Numero = iuo_TipoMovtoFruta.ultimocorrel(li_Planta,li_TipoMovto) 
	
	IF ll_Numero = 0 THEN
		RETURN
	ELSE
		dw_2.Object.mfco_numero[1] = ll_numero
	END IF	

	ll_Fila	=	dw_1.InsertRow(0)

	dw_1.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
	dw_1.Object.tpmv_codigo[ll_Fila]	=	li_TipoMovtoEnv
	dw_1.Object.meen_numero[ll_Fila]	=  ll_Numenva
	dw_1.Object.plde_coorde[ll_Fila]	=	dw_2.Object.plde_coorde[1]
	dw_1.Object.prod_codigo[ll_Fila]	=	dw_2.Object.prod_codigo[1]
	
	dw_1.Object.meen_guisii[ll_Fila]	=	dw_2.Object.mfco_guisii[1]
	dw_1.Object.meen_fecmov[ll_Fila]	=	dw_2.Object.mfco_fecmov[1]
	dw_1.Object.tran_codigo[ll_Fila]	=	dw_2.Object.tran_codigo[1]
	dw_1.Object.cami_clasifi[ll_Fila]=	dw_2.Object.cami_clasifi[1]
	dw_1.Object.cami_patent[ll_Fila]	=	dw_2.Object.cami_patent[1]
	dw_1.Object.cami_patcar[ll_Fila]	=	dw_2.Object.cami_patcar[1]
	dw_1.Object.meen_rutcho[ll_Fila]	=	dw_2.Object.mfco_rutcho[1]
	dw_1.Object.meen_chofer[ll_Fila]	=	dw_2.Object.mfco_chofer[1]
	dw_1.Object.meen_modulo[ll_Fila]	=  2
	dw_1.Object.tpmv_codrec[ll_fila] =  li_TipoMovto
	dw_1.Object.mfge_numero[ll_fila] =  ll_Numero

END IF

SELECT	IsNull(Max(mfcd_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dba.spro_movtofrutacomdeta
	WHERE	plde_codigo	=	:li_Planta
	AND	tpmv_codigo	=	:li_TipoMovto
	AND	mfco_numero	=	:ll_Numero ;

FOR ll_Fila = 1 TO dw_3.RowCount()
	IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_3.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_3.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_3.Object.mfco_numero[ll_Fila]	=	dw_2.Object.mfco_numero[1]
		dw_3.Object.mfcd_secuen[ll_Fila]	=	li_Secuencia 
		li_Secuencia ++
	END IF
NEXT

FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_4.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_4.Object.tpmv_codigo[ll_Fila]	=	li_TipoMovtoEnv
		dw_4.Object.meen_numero[ll_Fila]	=	ll_Numenva
		
	END IF
NEXT

FOR ll_Fila = 1 TO dw_5.RowCount()
	IF dw_5.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_5.Object.lofc_pltcod[ll_Fila]	=	dw_2.Object.plde_codigo[1]
	END IF
NEXT

FOR ll_Fila = 1 TO dw_6.RowCount()
	IF dw_6.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_6.Object.lofc_pltcod[ll_Fila]	=	dw_2.Object.plde_codigo[1]
	END IF
NEXT

iuo_TipoMovtoFruta.Actualiza(li_Planta,li_TipoMovto,ll_Numero) 
iuo_TipoMovtoEnva.Actualiza(li_Planta,li_TipoMovtoEnv,ll_Numenva) 
end event

event ue_borrar();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar		=	True
ib_AutoCommit	=	sqlca.AutoCommit

w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN
	dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)
END IF

IF dw_4.RowCount() > 0 THEN
	dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
END IF

IF dw_2.DeleteRow(0) = 1 AND dw_1.DeleteRow(0) = 1 THEN
		ib_Borrar	=	False
		
		w_main.SetMicroHelp("Borrando Registro...")
		
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_Borrar	=	False
	
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

event resize;call super::resize;tab_1.x			=	dw_1.x
tab_1.y			=	dw_1.y
tab_1.Height	=	dw_1.Height

end event

event ue_seleccion();call super::ue_seleccion;Str_Busqueda	lstr_Busq

lstr_Busq.Argum[1] = istr_Mant.Argumento[1]		// Código Planta
lstr_Busq.Argum[2] = istr_Mant.Argumento[2]		// Tipo de Movimiento
lstr_Busq.Argum[3] = ''									// Estado Movimiento
lstr_Busq.Argum[4] = ''  								// Fecha Inicio Movimiento

OpenWithParm(w_busc_movtofrutacomenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	istr_Mant.Argumento[1]	=	lstr_Busq.Argum[1]
	istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]

	This.TriggerEvent("ue_recuperadatos")
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtotraspasointerno
boolean visible = false
integer x = 59
integer y = 928
integer width = 3095
integer height = 940
string title = ""
string dataobject = "dw_mant_movtoenvaenca"
boolean hscrollbar = false
boolean vscrollbar = false
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtotraspasointerno
integer x = 37
integer y = 32
integer width = 3095
integer height = 856
integer taborder = 10
string dataobject = "dw_mant_movtofrutacomenca_traspasos"
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna
Integer	li_ClasifCamion

ls_Columna = GetColumnName()
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
		
	CASE "mfco_numero"
		IF NOT ExisteMovimiento(gstr_ParamPlanta.CodigoPlanta, &
									   Integer(istr_Mant.Argumento[2]), &
									   Integer(Data)) THEN
			This.SetItem(1,"mfco_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "plde_codigo"
		IF Not iuo_Planta.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)			
			RETURN 1
		ELSE
			Istr_Mant.Argumento[1] = String(iuo_planta.codigo)
		END IF
		
	CASE "plde_coorde"
		IF Not iuo_PltaDestino.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)			
			RETURN 1
		ELSEIF iuo_PltaDestino.Codigo = gstr_ParamPlanta.CodigoPlanta THEN
			MessageBox("Atención", "Planta Destino no Puede Ser Igual a la de Origen.~r~r" + &
						"Ingrese o seleccione otra Planta de Destino.", Exclamation!, Ok!)
			
			RETURN 1
		ELSE
			Istr_Mant.Argumento[5] = String(iuo_PltaDestino.Codigo)
		END IF

	CASE "prod_codigo"
		
		IF Not iuo_Productor.Existe(long(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, integer(ls_Nula))
			
			RETURN 1
		END IF
		
	CASE "tran_codigo"
		IF Not iuo_Transport.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			
			RETURN 1
		END IF

	CASE "cami_patent"
		
		IF Integer(istr_Mant.Argumento[2]) = 2 OR Integer(istr_Mant.Argumento[2]) = 22 THEN
			li_ClasifCamion	=	2
		ELSE
			li_ClasifCamion	=	1
		END IF

		IF Not iuo_Camion.Existe(li_ClasifCamion, Data, False, sqlca) THEN
			This.Object.cami_patcar[1]		=	ls_Nula
			This.Object.mfco_rutcho[1]		=	ls_Nula
			This.Object.mfco_chofer[1]		=	ls_Nula
		ELSE
			This.Object.cami_patcar[1]		=	iuo_Camion.PateCarro
			This.Object.mfco_rutcho[1]		=	iuo_Camion.RutChofer
			This.Object.mfco_chofer[1]		=	iuo_Camion.Chofer
			This.Object.cami_clasifi[1]	=	iuo_Camion.Clasificacion
		END IF

	CASE "defg_tipdoc"
		IF Not iuo_TipoDocto.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			RETURN 1
		END IF

	CASE "mfco_rutcho"
		is_rut = F_verrut(data, True)
		IF is_rut = "" THEN
			dw_2.SetItem(1, "mfco_rutcho", ls_Nula)
			RETURN 1
		ELSE
			This.SetItem(1, "mfco_rutcho", is_rut)
		END IF

	
END CHOOSE

HabilitaIngreso(ls_Columna)
end event

event dw_2::doubleclicked;//
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	IF Len(is_rut) < 10 THEN
		This.Object.mfco_rutcho.Format = '@@@@@@'
	ELSE
		This.Object.mfco_rutcho.Format = '@@@.@@@.@@@-@'
	END IF
	
	IF dwo.Name <> "mfco_rutcho" THEN
		This.SetItem(1, "mfco_rutcho", is_rut)
	END IF
END IF

IF is_rutcliente <> "" THEN
	IF Len(is_rutcliente ) < 10 THEN
		This.Object.clpr_rut.Format = '@@@@@@'
	ELSE
		This.Object.clpr_rut.Format = '@@@.@@@.@@@-@'
	END IF
	
	IF dwo.Name <> "clpr_rut" THEN
		This.SetItem(1, "clpr_rut", is_rutcliente )
	END IF
END IF
end event

event dw_2::clicked;CHOOSE CASE dwo.Name

	CASE "b_camion"
		BuscaCamion()

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtotraspasointerno
integer x = 3287
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtotraspasointerno
integer x = 3287
integer taborder = 70
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtotraspasointerno
integer x = 3287
integer y = 632
integer taborder = 80
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtotraspasointerno
boolean visible = false
integer x = 3287
integer taborder = 90
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtotraspasointerno
integer x = 3287
integer taborder = 100
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtotraspasointerno
integer x = 3287
integer y = 1504
integer taborder = 30
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtotraspasointerno
integer x = 3287
integer y = 1680
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtotraspasointerno
integer x = 3287
integer taborder = 50
end type

type tab_1 from tab within w_maed_movtotraspasointerno
event create ( )
event destroy ( )
integer x = 37
integer y = 944
integer width = 3095
integer height = 952
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.Control[]={this.tp_1,&
this.tp_2}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
end on

event selectionchanged;IF NewIndex = 1 THEN
	IF dw_3.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		il_Fila 					=	1
		
		dw_3.SelectRow(0,False)
		dw_4.SelectRow(0,False)
		dw_3.SetRow(il_Fila)
		dw_3.SelectRow(il_Fila, True)
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
ELSE
	IF dw_4.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		il_Fila 					=	1
		
		dw_3.SelectRow(0,False)
		dw_4.SelectRow(0,False)
		dw_4.SetRow(il_Fila)
		dw_4.SelectRow(il_Fila, True)
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
END IF
end event

type tp_1 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 3058
integer height = 824
boolean enabled = false
long backcolor = 12632256
string text = "Detalle de Fruta"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Table!"
long picturemaskcolor = 553648127
dw_detalle dw_detalle
end type

on tp_1.create
this.dw_detalle=create dw_detalle
this.Control[]={this.dw_detalle}
end on

on tp_1.destroy
destroy(this.dw_detalle)
end on

type dw_detalle from uo_dw within tp_1
integer x = 37
integer y = 36
integer width = 2981
integer height = 684
integer taborder = 11
string dataobject = "dw_mues_movtofrutacomdeta_movto"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtotraspasointerno.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutacomenca.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomenca.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event getfocus;call super::getfocus;IF il_fila > 0 THEN This.SelectRow(il_fila, True)

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

type tp_2 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 3058
integer height = 824
boolean enabled = false
long backcolor = 12632256
string text = "Detalle de Envases       "
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "ArrangeTables!"
long picturemaskcolor = 553648127
dw_envases dw_envases
end type

on tp_2.create
this.dw_envases=create dw_envases
this.Control[]={this.dw_envases}
end on

on tp_2.destroy
destroy(this.dw_envases)
end on

type dw_envases from uo_dw within tp_2
integer x = 37
integer y = 36
integer width = 2981
integer height = 684
integer taborder = 10
string dataobject = "dw_mues_movtoenvadeta"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtotraspasointerno.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutacomenca.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomenca.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event getfocus;call super::getfocus;IF il_fila > 0 THEN This.SelectRow(il_fila, True)

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

type cb_capturadatos from commandbutton within w_maed_movtotraspasointerno
boolean visible = false
integer x = 3232
integer y = 1196
integer width = 288
integer height = 112
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Captura"
end type

event clicked;String	ls_directorio
Integer	li_valida, li_opcion = 1
dwitemstatus stat

ib_ok	= True

IF dw_3.AcceptText() = -1 THEN li_opcion = -1
IF dw_3.ModifiedCount() > 0 THEN 
	li_opcion = 0
END IF

CHOOSE CASE li_opcion
	CASE -1
		ib_ok = False
	CASE 0
		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
			CASE 1
				Message.DoubleParm = 0
				This.triggerevent("ue_guardar")
				IF message.doubleparm = -1 THEN ib_ok = False
				RETURN
			CASE 3
				ib_ok	= False
				RETURN
		END CHOOSE
END CHOOSE

IF ib_ok = False THEN RETURN

pb_nuevo.TriggerEvent(Clicked!)

DO
	li_valida	= GetFileOpenName("Carga de Archivo", ls_directorio, is_archivo, "emb", &
											"Archivos Fruta Comercial (*.FCM), *.FCM,Todos los Archivos (*.*), *.*")
	IF li_valida = 0 THEN
		pb_salir.SetFocus()
		RETURN
	ELSEIF li_valida = -1 THEN
		MessageBox("Error de Apertura","Ocurrió un error al utilizar el archivo",Information!,Ok!)
		Message.DoubleParm = 1
	ELSE
		is_archivo	= ls_directorio

		Message.DoubleParm = 2
		Parent.TriggerEvent("ue_carga_detalle")

		tab_1.tp_1.Enabled	=	True
		tab_1.tp_2.Enabled	=	True
	END IF
	
LOOP WHILE Message.DoubleParm = 1

IF dw_4.RowCount() > 0 THEN
	pb_grabar.Enabled = True
ELSE
	pb_grabar.Enabled = False
END IF
end event

type dw_6 from datawindow within w_maed_movtotraspasointerno
boolean visible = false
integer x = 78
integer y = 1400
integer width = 3054
integer height = 260
integer taborder = 70
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle Lote"
string dataobject = "dw_gene_spro_lotesfrutacomdeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_maed_movtotraspasointerno
boolean visible = false
integer x = 114
integer y = 1640
integer width = 3054
integer height = 260
integer taborder = 10
boolean bringtotop = true
boolean titlebar = true
string title = "Encabezado Lote"
string dataobject = "dw_gene_spro_lotesfrutacomenc"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

