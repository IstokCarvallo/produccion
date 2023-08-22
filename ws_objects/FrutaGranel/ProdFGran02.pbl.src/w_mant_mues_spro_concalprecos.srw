$PBExportHeader$w_mant_mues_spro_concalprecos.srw
$PBExportComments$Ingreso de Documentos Internos Packing
forward
global type w_mant_mues_spro_concalprecos from window
end type
type pb_buscar from picturebutton within w_mant_mues_spro_concalprecos
end type
type pb_imprimir from picturebutton within w_mant_mues_spro_concalprecos
end type
type pb_nuevo from picturebutton within w_mant_mues_spro_concalprecos
end type
type pb_guardar from picturebutton within w_mant_mues_spro_concalprecos
end type
type pb_salir from picturebutton within w_mant_mues_spro_concalprecos
end type
type dw_1 from datawindow within w_mant_mues_spro_concalprecos
end type
type gb_1 from groupbox within w_mant_mues_spro_concalprecos
end type
end forward

global type w_mant_mues_spro_concalprecos from window
string tag = "w_mant_mues_spro_concalprecos"
integer width = 3547
integer height = 1988
boolean titlebar = true
string title = "Control Calidad PreCosecha"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 12632256
event ue_guardar pbm_custom11
event ue_imprimir ( )
event ue_antesguardar ( )
event ue_validaguardar ( )
event ue_recuperadatos ( )
event ue_habilitaencab ( )
event ue_deshabilitaencab ( )
event ue_nuevo ( )
event ue_modifica ( )
event ue_deshabilitadeta ( )
event ue_habilitadeta ( )
event ue_seleccion ( )
pb_buscar pb_buscar
pb_imprimir pb_imprimir
pb_nuevo pb_nuevo
pb_guardar pb_guardar
pb_salir pb_salir
dw_1 dw_1
gb_1 gb_1
end type
global w_mant_mues_spro_concalprecos w_mant_mues_spro_concalprecos

type variables
str_mant				istr_mant

uo_Paramprecos		iuo_Paramprecos
uo_productores    iuo_productor
uo_variedades     iuo_variedad
uo_predios        iuo_predio


DataWindowChild  	idwc_pltadesp, idwc_tipdoc, idwc_numero, idwc_planta,&
                  idwc_especie, idwc_variedad, idwc_serplanta, &
						idwc_linea, idwc_predio

end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public function boolean existespecie (integer ai_especie)
public function boolean duplicadopk (integer ai_planta, integer ai_numero)
end prototypes

event type long ue_guardar(unsignedlong wparam, long lparam);IF dw_1.AcceptText() = -1 THEN RETURN -1

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

This.TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN -1

IF duplicadopk(dw_1.object.plde_codigo[1],dw_1.object.ccpr_folio[1]) = false THEN
	IF wf_actualiza_db() THEN
		w_main.SetMicroHelp("Información Grabada.")
		TriggerEvent("ue_Deshabilitaencab")
	ELSE
		w_main.SetMicroHelp("No se puede Grabar información.")
		Message.DoubleParm = -1
		RETURN -1
	END IF
ELSE
	dw_1.update()
	TriggerEvent("ue_Deshabilitaencab")
END IF

RETURN 0
end event

event ue_imprimir();SetPointer(HourGlass!)

//Long		fila
//str_info	lstr_info
//
//lstr_info.titulo	= "DOCUMENTOS INTERNOS PACKING"
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
////vinf.dw_1.DataObject = "dw_info_doctointernopack"
//
//vinf.dw_1.SetTransObject(sqlca)
//
//fila = vinf.dw_1.Retrieve()
//
//IF fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
//ELSEIF fila = 0 THEN
//	MessageBox( "No Existe información", "No Existe información para este informe.", &
//					StopSign!, OK!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//	
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//SetPointer(Arrow!)
end event

event ue_antesguardar();Integer 	il_fila, il_cont
Long		ll_fila
String	ls_mensaje, ls_columna, ls_fecha

dw_1.accepttext()

IF Isnull(dw_1.GetItemNumber(1,"ccpr_folio")) or &
	String(dw_1.GetItemNumber(1,"ccpr_folio")) = "" THEN
	ls_mensaje = "Número de Folio"
	il_cont++
END IF

IF Isnull(dw_1.GetItemNumber(1,"espe_codigo")) or&
	String(dw_1.GetItemNumber(1,"espe_codigo")) = "" THEN
	ls_mensaje = "Especie "
	il_cont++
END IF

/* Se rescata la fecha desde la datawindows para luego ver si es que*/
					/* es nula o fue mal ingresada*/

ls_fecha = string(dw_1.Object.ccpr_fechai[1])

IF Isnull(ls_fecha) or (ls_fecha) = "00/00/0000" THEN
	ls_mensaje = "Fecha"
	il_cont++
END IF

IF Isnull(dw_1.GetItemNumber(1,"vari_codigo")) or&
	String(dw_1.GetItemNumber(1,"vari_codigo")) = "" THEN
	ls_mensaje = "Variedad "
	il_cont++
END IF

IF Isnull(dw_1.GetItemNumber(1,"ccpr_diadpf")) or&
	String(dw_1.GetItemNumber(1,"ccpr_diadpf")) = "" THEN
	ls_mensaje = "DDFF "
	il_cont++
END IF

IF il_cont  = 1 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetFocus()
	Message.DoubleParm = -1
	RETURN 
ELSEIF il_cont > 1 THEN
	ls_mensaje = "Datos"
	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetFocus()
	Message.DoubleParm = -1
	RETURN 
END IF
end event

event ue_recuperadatos();Long	ll_fila, respuesta

dw_1.accepttext()

istr_mant.argumento[1] = String(dw_1.Object.plde_codigo[1])
istr_mant.argumento[2] = String(dw_1.Object.ccpr_folio[1])

DO 
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[1]),&
									Integer(istr_mant.argumento[2]))
				  
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.",& 
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		
		dw_1.getChild("vari_codigo",idwc_variedad)
		idwc_variedad.SetTransObject(SQLCA)
		IF idwc_variedad.Retrieve(dw_1.Object.espe_codigo[1])=0 THEN
			idwc_variedad.InsertRow(0)
		END IF	
		
		dw_1.getChild("prbr_codpre",idwc_predio)
		idwc_predio.SetTransObject(SQLCA)
		IF idwc_predio.Retrieve(dw_1.Object.prod_codigo[1])=0 THEN
			idwc_predio.InsertRow(0)
		END IF
		
		dw_1.SetRow(1)
		dw_1.SetFocus()
	   TriggerEvent("ue_deshabilitaencab")
		TriggerEvent("ue_deshabilitadeta")
		pb_imprimir.Enabled = TRUE
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

dw_1.SetFocus()


			


end event

event ue_habilitaencab();
dw_1.Object.ccpr_folio.Protect	=	0
dw_1.Modify("ccpr_folio.BackGround.Color = " + String(rgb(255,255,255)))

dw_1.Object.espe_codigo.Protect	=	0 
dw_1.Modify("espe_codigo.BackGround.Color = " + String(rgb(255,255,255)))


end event

event ue_deshabilitaencab();
dw_1.Object.ccpr_folio.Protect=1
dw_1.Modify("ccpr_folio.BackGround.Color = " + String(rgb(192,192,192)))

dw_1.Object.espe_codigo.Protect=1
dw_1.Modify("espe_codigo.BackGround.Color = " + String(rgb(192,192,192)))


end event

event ue_nuevo();TriggerEvent("ue_habilitaencab")




end event

event ue_deshabilitadeta();IF iuo_ParamPrecos.Existe(Integer(dw_1.object.espe_codigo[1]),true,sqlca) then
   IF iuo_ParamPrecos.pesfru = 0 THEN
		dw_1.Object.ccpr_pespro.Protect 				=	1
		dw_1.Object.ccpr_pespro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_pesmin.Protect 				=	1
		dw_1.Object.ccpr_pesmin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_pesmax.Protect 				=	1
		dw_1.Object.ccpr_pesmax.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamPrecos.calibr = 0 THEN
		dw_1.Object.ccpr_calpro.Protect 				=	1
		dw_1.Object.ccpr_calpro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_calmin.Protect 				=	1
		dw_1.Object.ccpr_calmin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_calmax.Protect 				=	1
		dw_1.Object.ccpr_calmax.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamPrecos.colvis = 0 THEN
		dw_1.Object.ccpr_cvipro.Protect 				=	1
		dw_1.Object.ccpr_cvipro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_cvimin.Protect 				=	1
		dw_1.Object.ccpr_cvimin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_cvimax.Protect 				=	1
		dw_1.Object.ccpr_cvimax.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamPrecos.colfon = 0 THEN
		dw_1.Object.ccpr_cfopro.Protect 				=	1
		dw_1.Object.ccpr_cfopro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_cfomin.Protect 				=	1
		dw_1.Object.ccpr_cfomin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_cfomax.Protect 				=	1
		dw_1.Object.ccpr_cfomax.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamPrecos.calabi = 0 THEN
		dw_1.Object.ccpr_cabpro.Protect 				=	1
		dw_1.Object.ccpr_cabpro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_cabmin.Protect 				=	1
		dw_1.Object.ccpr_cabmin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_cabmax.Protect 				=	1
		dw_1.Object.ccpr_cabmax.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamPrecos.presio = 0 THEN
		dw_1.Object.ccpr_prppro.Protect 				=	1
		dw_1.Object.ccpr_prppro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_prpmin.Protect 				=	1
		dw_1.Object.ccpr_prpmin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_prpmax.Protect 				=	1
		dw_1.Object.ccpr_prpmax.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamPrecos.pca100 = 0 THEN
		dw_1.Object.ccpr_pcapro.Protect 				=	1
		dw_1.Object.ccpr_pcapro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_pcamin.Protect 				=	1
		dw_1.Object.ccpr_pcamin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_pcamax.Protect 				=	1
		dw_1.Object.ccpr_pcamax.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamPrecos.colsem = 0 THEN
		dw_1.Object.ccpr_pcapro.Protect 				=	1
		dw_1.Object.ccpr_pcapro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_pcamin.Protect 				=	1
		dw_1.Object.ccpr_pcamin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_pcamax.Protect 				=	1
		dw_1.Object.ccpr_pcamax.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamPrecos.nrosem = 0 THEN
		dw_1.Object.ccpr_nsepro.Protect 				=	1
		dw_1.Object.ccpr_nsepro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_nsemin.Protect 				=	1
		dw_1.Object.ccpr_nsemin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_nsemax.Protect 				=	1
		dw_1.Object.ccpr_nsemax.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamPrecos.almido = 0 THEN
		dw_1.Object.ccpr_almpro.Protect 				=	1
		dw_1.Object.ccpr_almpro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_almmin.Protect 				=	1
		dw_1.Object.ccpr_almmin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_almmax.Protect 				=	1
		dw_1.Object.ccpr_almmax.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamPrecos.acidez = 0 THEN
		dw_1.Object.ccpr_acipro.Protect 				=	1
		dw_1.Object.ccpr_acipro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_acimin.Protect 				=	1
		dw_1.Object.ccpr_acimin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_acimax.Protect 				=	1
		dw_1.Object.ccpr_acimax.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamPrecos.coracu = 0 THEN
		dw_1.Object.ccpr_cacpro.Protect 				=	1
		dw_1.Object.ccpr_cacpro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_cacmin.Protect 				=	1
		dw_1.Object.ccpr_cacmin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_cacmax.Protect 				=	1
		dw_1.Object.ccpr_cacmax.BackGround.Color	=	RGB(192,192,192)
   END IF
   
	IF iuo_ParamPrecos.cormoh = 0 THEN
		dw_1.Object.ccpr_cmopro.Protect 				=	1
		dw_1.Object.ccpr_cmopro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_cmomin.Protect 				=	1
		dw_1.Object.ccpr_cmomin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_cmomax.Protect 				=	1
		dw_1.Object.ccpr_cmomax.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamPrecos.solsol = 0 THEN
		dw_1.Object.ccpr_ssipro.Protect 				=	1
		dw_1.Object.ccpr_ssipro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_ssimin.Protect 				=	1
		dw_1.Object.ccpr_ssimin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_ssimax.Protect 				=	1
		dw_1.Object.ccpr_ssimax.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamPrecos.sosofi = 0 THEN
		dw_1.Object.ccpr_ssfpro.Protect 				=	1
		dw_1.Object.ccpr_ssfpro.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_ssfmin.Protect 				=	1
		dw_1.Object.ccpr_ssfmin.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpr_ssfmax.Protect 				=	1
		dw_1.Object.ccpr_ssfmax.BackGround.Color	=	RGB(192,192,192)
   END IF
	
	IF iuo_ParamPrecos.matsec = 0 THEN
		dw_1.Object.ccpr_matsec.Protect 				=	1
		dw_1.Object.ccpr_matsec.BackGround.Color	=	RGB(192,192,192)
	END IF
	
   IF iuo_ParamPrecos.ph = 0 THEN
		dw_1.Object.ccpr_ph.Protect 				=	1
		dw_1.Object.ccpr_ph.BackGround.Color	=	RGB(192,192,192)
	END IF
	
   IF iuo_ParamPrecos.gasto= 0 THEN
		dw_1.Object.ccpr_gasto.Protect 				=	1
		dw_1.Object.ccpr_gasto.BackGround.Color	=	RGB(192,192,192)
	END IF

END IF

end event

event ue_habilitadeta();   IF iuo_ParamPrecos.pesfru = 0 THEN
		dw_1.Object.ccpr_pespro.Protect 				=	0
		dw_1.Object.ccpr_pespro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_pesmin.Protect 				=	0
		dw_1.Object.ccpr_pesmin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_pesmax.Protect 				=	0
		dw_1.Object.ccpr_pesmax.BackGround.Color	=	RGB(255,255,255)
   END IF
	
   IF iuo_ParamPrecos.calibr = 0 THEN
		dw_1.Object.ccpr_calpro.Protect 				=	0
		dw_1.Object.ccpr_calpro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_calmin.Protect 				=	0
		dw_1.Object.ccpr_calmin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_calmax.Protect 				=	0
		dw_1.Object.ccpr_calmax.BackGround.Color	=	RGB(255,255,255)
   END IF

   IF iuo_ParamPrecos.colvis = 0 THEN
		dw_1.Object.ccpr_cvipro.Protect 				=	0
		dw_1.Object.ccpr_cvipro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_cvimin.Protect 				=	0
		dw_1.Object.ccpr_cvimin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_cvimax.Protect 				=	0
		dw_1.Object.ccpr_cvimax.BackGround.Color	=	RGB(255,255,255)
   END IF

   IF iuo_ParamPrecos.colfon = 0 THEN
		dw_1.Object.ccpr_cfopro.Protect 				=	0
		dw_1.Object.ccpr_cfopro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_cfomin.Protect 				=	0
		dw_1.Object.ccpr_cfomin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_cfomax.Protect 				=	0
		dw_1.Object.ccpr_cfomax.BackGround.Color	=	RGB(255,255,255)
   END IF
	
   IF iuo_ParamPrecos.calabi = 0 THEN
		dw_1.Object.ccpr_cabpro.Protect 				=	0
		dw_1.Object.ccpr_cabpro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_cabmin.Protect 				=	0
		dw_1.Object.ccpr_cabmin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_cabmax.Protect 				=	0
		dw_1.Object.ccpr_cabmax.BackGround.Color	=	RGB(255,255,255)
   END IF

   IF iuo_ParamPrecos.presio = 0 THEN
		dw_1.Object.ccpr_prppro.Protect 				=	0
		dw_1.Object.ccpr_prppro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_prpmin.Protect 				=	0
		dw_1.Object.ccpr_prpmin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_prpmax.Protect 				=	0
		dw_1.Object.ccpr_prpmax.BackGround.Color	=	RGB(255,255,255)
   END IF

   IF iuo_ParamPrecos.pca100 = 0 THEN
		dw_1.Object.ccpr_pcapro.Protect 				=	0
		dw_1.Object.ccpr_pcapro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_pcamin.Protect 				=	0
		dw_1.Object.ccpr_pcamin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_pcamax.Protect 				=	0
		dw_1.Object.ccpr_pcamax.BackGround.Color	=	RGB(255,255,255)
   END IF
	
   IF iuo_ParamPrecos.colsem = 0 THEN
		dw_1.Object.ccpr_pcapro.Protect 				=	0
		dw_1.Object.ccpr_pcapro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_pcamin.Protect 				=	0
		dw_1.Object.ccpr_pcamin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_pcamax.Protect 				=	0
		dw_1.Object.ccpr_pcamax.BackGround.Color	=	RGB(255,255,255)
   END IF

   IF iuo_ParamPrecos.nrosem = 0 THEN
		dw_1.Object.ccpr_nsepro.Protect 				=	0
		dw_1.Object.ccpr_nsepro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_nsemin.Protect 				=	0
		dw_1.Object.ccpr_nsemin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_nsemax.Protect 				=	0
		dw_1.Object.ccpr_nsemax.BackGround.Color	=	RGB(255,255,255)
   END IF
	
   IF iuo_ParamPrecos.almido = 0 THEN
		dw_1.Object.ccpr_almpro.Protect 				=	0
		dw_1.Object.ccpr_almpro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_almmin.Protect 				=	0
		dw_1.Object.ccpr_almmin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_almmax.Protect 				=	0
		dw_1.Object.ccpr_almmax.BackGround.Color	=	RGB(255,255,255)
   END IF
	
   IF iuo_ParamPrecos.acidez = 0 THEN
		dw_1.Object.ccpr_acipro.Protect 				=	0
		dw_1.Object.ccpr_acipro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_acimin.Protect 				=	0
		dw_1.Object.ccpr_acimin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_acimax.Protect 				=	0
		dw_1.Object.ccpr_acimax.BackGround.Color	=	RGB(255,255,255)
   END IF

   IF iuo_ParamPrecos.coracu = 0 THEN
		dw_1.Object.ccpr_cacpro.Protect 				=	0
		dw_1.Object.ccpr_cacpro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_cacmin.Protect 				=	0
		dw_1.Object.ccpr_cacmin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_cacmax.Protect 				=	0
		dw_1.Object.ccpr_cacmax.BackGround.Color	=	RGB(255,255,255)
   END IF
   
	IF iuo_ParamPrecos.cormoh = 0 THEN
		dw_1.Object.ccpr_cmopro.Protect 				=	0
		dw_1.Object.ccpr_cmopro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_cmomin.Protect 				=	0
		dw_1.Object.ccpr_cmomin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_cmomax.Protect 				=	0
		dw_1.Object.ccpr_cmomax.BackGround.Color	=	RGB(255,255,255)
   END IF
	
   IF iuo_ParamPrecos.solsol = 0 THEN
		dw_1.Object.ccpr_ssipro.Protect 				=	0
		dw_1.Object.ccpr_ssipro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_ssimin.Protect 				=	0
		dw_1.Object.ccpr_ssimin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_ssimax.Protect 				=	0
		dw_1.Object.ccpr_ssimax.BackGround.Color	=	RGB(255,255,255)
   END IF
	
   IF iuo_ParamPrecos.sosofi = 0 THEN
		dw_1.Object.ccpr_ssfpro.Protect 				=	0
		dw_1.Object.ccpr_ssfpro.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_ssfmin.Protect 				=	0
		dw_1.Object.ccpr_ssfmin.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpr_ssfmax.Protect 				=	0
		dw_1.Object.ccpr_ssfmax.BackGround.Color	=	RGB(255,255,255)
   END IF
	
	IF iuo_ParamPrecos.matsec = 0 THEN
		dw_1.Object.ccpr_matsec.Protect 				=	0
		dw_1.Object.ccpr_matsec.BackGround.Color	=	RGB(255,255,255)
	END IF
	
   IF iuo_ParamPrecos.ph = 0 THEN
		dw_1.Object.ccpr_ph.Protect 					=	0
		dw_1.Object.ccpr_ph.BackGround.Color		=	RGB(255,255,255)
	END IF
	
   IF iuo_ParamPrecos.gasto= 0 THEN
		dw_1.Object.ccpr_gasto.Protect 				=	0
		dw_1.Object.ccpr_gasto.BackGround.Color	=	RGB(255,255,255)
	END IF



end event

event ue_seleccion();str_busqueda	lstr_busq
String	ls_Null

lstr_busq.argum[1]	=	String(dw_1.object.plde_codigo[1])
lstr_busq.argum[2]	=  String(dw_1.object.espe_codigo[1])
lstr_busq.argum[3]	=  String(dw_1.object.vari_codigo[1])
lstr_busq.argum[4]	=  String(dw_1.object.prod_codigo[1])

OpenWithParm(w_busc_spro_concalprecos, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = ""  THEN
	dw_1.SetColumn("ccpr_folio")
	dw_1.SetFocus()
ELSE
	dw_1.setitem(1,"plde_codigo",integer(lstr_busq.argum[1]))
	dw_1.SetItem(1,"ccpr_folio", Long(lstr_busq.argum[2]))
	TriggerEvent("ue_recuperadatos")
END IF

RETURN
end event

protected function boolean wf_actualiza_db ();if dw_1.update() = 1 then 
	commit;
	if sqlca.sqlcode <> 0 then
		F_ErrorBaseDatos(sqlca,this.title)
		return false
	else
		return true
	end if 
else
	rollback;
	if sqlca.sqlcode <> 0 then F_ErrorBaseDatos(sqlca,this.title)
	return false
end if
return true
end function

public function boolean existespecie (integer ai_especie);//Función que comprueba si existe la especie el la tabla spro_concalparmadprecos

Integer	li_Contador

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.spro_concalparmadprecos
	WHERE	espe_codigo	=	:ai_especie;


IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla spro_concalparmadprecos")
	RETURN True
ELSEIF li_Contador > 0 THEN
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean duplicadopk (integer ai_planta, integer ai_numero);Integer	li_Contador

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.spro_concalprecos
	WHERE	plde_codigo	=	:ai_Planta
	AND	ccpr_folio	=	:ai_Numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla spro_concalprecos")
	RETURN True
ELSEIF li_Contador > 0 THEN
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_mues_spro_concalprecos.create
this.pb_buscar=create pb_buscar
this.pb_imprimir=create pb_imprimir
this.pb_nuevo=create pb_nuevo
this.pb_guardar=create pb_guardar
this.pb_salir=create pb_salir
this.dw_1=create dw_1
this.gb_1=create gb_1
this.Control[]={this.pb_buscar,&
this.pb_imprimir,&
this.pb_nuevo,&
this.pb_guardar,&
this.pb_salir,&
this.dw_1,&
this.gb_1}
end on

on w_mant_mues_spro_concalprecos.destroy
destroy(this.pb_buscar)
destroy(this.pb_imprimir)
destroy(this.pb_nuevo)
destroy(this.pb_guardar)
destroy(this.pb_salir)
destroy(this.dw_1)
destroy(this.gb_1)
end on

event open;Long ll_fila

x=0
y=0

iuo_ParamPrecos	=	Create uo_Paramprecos
iuo_productor		=	Create uo_productores    
iuo_variedad      =  Create uo_variedades     
iuo_predio        =  Create uo_predios        

//Rescata el codigo de planta
istr_Mant.Argumento[1]		= String(gstr_ParamPlanta.CodigoPlanta)

//Retrieve para la datawindows que muestra las especies
dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()

dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.InsertRow(0)

dw_1.GetChild("prbr_codpre", idwc_predio)
idwc_predio.SetTransObject(sqlca)
idwc_predio.InsertRow(0)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetTransObject(sqlca)

dw_1.InsertRow(0)
dw_1.Setitem(1,"plde_codigo",gstr_ParamPlanta.CodigoPlanta)

pb_guardar.enabled  = False
pb_imprimir.enabled = False

dw_1.setcolumn("ccpr_folio")

end event

event closequery;dw_1.accepttext()
IF dw_1.modifiedcount() > 0 THEN 
	CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
		CASE 1
			Message.DoubleParm = 0
			triggerevent("ue_guardar")
			IF message.doubleparm = -1 THEN Message.ReturnValue = 1
			RETURN 
		CASE 3
			Message.ReturnValue = 1
			RETURN
	END CHOOSE
END IF
end event

type pb_buscar from picturebutton within w_mant_mues_spro_concalprecos
event ue_mousemove pbm_mousemove
integer x = 3269
integer y = 92
integer width = 155
integer height = 132
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\Buscae.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_seleccion")

end event

type pb_imprimir from picturebutton within w_mant_mues_spro_concalprecos
event ue_mousemove pbm_mousemove
string tag = "Imprimir Informacion"
integer x = 3269
integer y = 632
integer width = 155
integer height = 132
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo\Bmp\Imprimee.bmp"
string disabledname = "\Desarrollo\Bmp\Imprimed.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_imprimir")

end event

type pb_nuevo from picturebutton within w_mant_mues_spro_concalprecos
event ue_mousemove pbm_mousemove
integer x = 3269
integer y = 272
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\Nuevoe.bmp"
alignment htextalign = left!
end type

event clicked;dw_1.accepttext()
IF dw_1.modifiedcount() > 0 THEN 
	CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
		CASE 1
			Message.DoubleParm = 0
			Parent.triggerevent("ue_guardar")
			IF message.doubleparm = -1 THEN Message.ReturnValue = 1
			RETURN 
		CASE 3
			Parent.TriggerEvent("ue_habilitaencab")
			Parent.TriggerEvent("ue_habilitadeta")
			Message.ReturnValue = 1
			RETURN
	END CHOOSE
END IF	

Parent.TriggerEvent("ue_habilitaencab")
Parent.TriggerEvent("ue_habilitadeta")

dw_1.Reset()
dw_1.insertRow(0)
pb_imprimir.Enabled = False
pb_guardar.Enabled  = False

dw_1.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta

dw_1.setcolumn("ccpr_folio")
end event

type pb_guardar from picturebutton within w_mant_mues_spro_concalprecos
event ue_mousemove pbm_mousemove
string tag = "Grabar Informacion"
integer x = 3269
integer y = 452
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo\Bmp\Disksave.bmp"
string disabledname = "\Desarrollo\Bmp\Disksavd.bmp"
alignment htextalign = left!
end type

event ue_mousemove;w_main.SetMicroHelp("Grabar actual información")
end event

event clicked;Parent.TriggerEvent("ue_guardar")

end event

type pb_salir from picturebutton within w_mant_mues_spro_concalprecos
event ue_mousemove pbm_mousemove
integer x = 3269
integer y = 812
integer width = 155
integer height = 132
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event ue_mousemove;w_main.SetMicroHelp("Salir de Parametros")
end event

event clicked;Close(Parent)
end event

type dw_1 from datawindow within w_mant_mues_spro_concalprecos
integer x = 32
integer y = 36
integer width = 3150
integer height = 1812
integer taborder = 10
string dataobject = "dw_mant_mues_spro_concalprecos"
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	planta, numerofol,li_null
String	ls_columna
SetNull(li_Null)

dw_1.accepttext()

planta 		= dw_1.Object.plde_codigo[dw_1.getrow()]
numerofol 	= dw_1.Object.ccpr_folio[dw_1.getrow()]
ls_columna  = GetColumnName()

CHOOSE CASE ls_columna
	
	CASE "ccpr_folio"

		IF Not(Isnull(dw_1.GetItemNumber(1,"plde_codigo"))) or &
			String(dw_1.GetItemNumber(1,"plde_codigo")) <> "" THEN
			IF duplicadopk(planta,numerofol) = true THEN
				parent.triggerevent("ue_recuperadatos")
			END IF
			pb_guardar.enabled  = True		
		END IF

	CASE "plde_codigo"
		
		IF Not(Isnull(dw_1.GetItemNumber(1,"ccpr_folio"))) or &
			String(dw_1.GetItemNumber(1,"ccpr_folio")) <> "" THEN
			IF duplicadopk(planta,numerofol) = true THEN
				Parent.triggerevent("ue_recuperadatos")
			END IF
		END IF

	CASE "espe_codigo" 
		
		IF Not(Isnull(dw_1.GetItemNumber(1,"plde_codigo"))) or &
			String(dw_1.GetItemNumber(1,"plde_codigo")) <> "" THEN
			IF existespecie(integer(data)) = true THEN
				Parent.Triggerevent("ue_habilitadeta")				
				Parent.Triggerevent("ue_deshabilitadeta")
				dw_1.GetChild("vari_codigo", idwc_variedad)
				idwc_variedad.SetTransObject(sqlca)
				IF idwc_variedad.Retrieve(integer(data))=0 THEN
					idwc_variedad.InsertRow(0)
			   END IF
			ELSE
				Messagebox("Error","Especie no ingresada en la PreCosecha") 
				dw_1.object.espe_codigo[1] = li_null
			END IF
		END IF

	CASE "prod_codigo" 
		
		IF Not iuo_productor.Existe(Long(data),True,Sqlca) THEN
			this.SetItem(1,"prod_codigo",li_Null)
			Return 1
		ELSE	
			dw_1.getChild("prbr_codpre",idwc_predio)
			idwc_predio.SetTransObject(SQLCA)
			IF idwc_predio.Retrieve(Long(data))=0 THEN
				idwc_predio.InsertRow(0)
			END IF
		END IF	
		
	CASE "prbr_codpre" 
		
		IF Not iuo_predio.Existe(Sqlca,True,dw_1.Object.prod_codigo[1],integer(data)) THEN
			this.SetItem(1,"prbr_codpre",li_Null)
			RETURN 1
      END IF
		
	CASE "vari_codigo" 
		
		IF Not iuo_variedad.Existe(dw_1.Object.espe_codigo[1],integer(data),True,Sqlca) THEN
			this.SetItem(1,"vari_codigo",li_Null)
			Return 1
		END IF
		
	CASE "ccpr_diasco"
		IF Integer(data) > 999 OR Integer(data) < 0 THEN
			This.SetItem(row, "ccpr_diasco", li_Null)
			RETURN 1
		END IF
		
	CASE "ccpr_totfru"
		IF Integer(data) > 99 OR Integer(data) < 0 THEN
			This.SetItem(row, "ccpr_totfru", li_Null)
			RETURN 1
		END IF
		
	CASE "ccpr_pespro", "ccpr_pesmin", "ccpr_pesmax", "ccpr_calpro", "ccpr_calmin", "ccpr_calmax", &
		  "ccpr_cvipro", "ccpr_cvimin", "ccpr_cvimax", "ccpr_cfopro", "ccpr_cfomin", "ccpr_cfomax", &
		  "ccpr_cabpro", "ccpr_cabmin", "ccpr_cabmax", "ccpr_prppro", "ccpr_prpmin", "ccpr_prpmax", &
		  "ccpr_pcapro", "ccpr_pcamin", "ccpr_pcamax", "ccpr_csepro", "ccpr_csemin", "ccpr_csemax", &
		  "ccpr_nsepro", "ccpr_nsemin", "ccpr_nsemax", "ccpr_almpro", "ccpr_almmin", "ccpr_almmax", &
		  "ccpr_acipro", "ccpr_acimin", "ccpr_acimax", "ccpr_cacpro", "ccpr_cacmin", "ccpr_cacmax", &
		  "ccpr_cmopro", "ccpr_cmomin", "ccpr_cmomax", "ccpr_ssipro", "ccpr_ssimin", "ccpr_ssimax", &
		  "ccpr_ssfpro", "ccpr_ssfmin", "ccpr_ssfmax", "ccpr_matsec", "ccpr_ph", "ccpr_gasto"
		IF Dec(data) >= 100 OR Dec(data) < 0 THEN
			This.SetItem(row, ls_Columna, Dec(li_Null))
			RETURN 1
		END IF
		  
		
END CHOOSE
end event

event itemerror;Return 1
end event

type gb_1 from groupbox within w_mant_mues_spro_concalprecos
integer x = 3205
integer y = 12
integer width = 279
integer height = 980
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

