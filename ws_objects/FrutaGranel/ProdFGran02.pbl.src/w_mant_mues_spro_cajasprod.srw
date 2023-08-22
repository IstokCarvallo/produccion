$PBExportHeader$w_mant_mues_spro_cajasprod.srw
$PBExportComments$Ingreso de Documentos Internos Packing
forward
global type w_mant_mues_spro_cajasprod from window
end type
type pb_ok from picturebutton within w_mant_mues_spro_cajasprod
end type
type sle_1 from singlelineedit within w_mant_mues_spro_cajasprod
end type
type dw_2 from datawindow within w_mant_mues_spro_cajasprod
end type
type pb_salir from picturebutton within w_mant_mues_spro_cajasprod
end type
type dw_1 from datawindow within w_mant_mues_spro_cajasprod
end type
type gb_1 from groupbox within w_mant_mues_spro_cajasprod
end type
type gb_2 from groupbox within w_mant_mues_spro_cajasprod
end type
end forward

global type w_mant_mues_spro_cajasprod from window
string tag = "w_mant_mues_spro_cajasprod"
integer width = 3534
integer height = 1716
boolean titlebar = true
string title = "Cajas en Producción"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 12632256
event ue_guardar pbm_custom11
event ue_imprimir ( )
event ue_antesguardar ( )
event ue_validaguardar ( )
event ue_modifica ( )
pb_ok pb_ok
sle_1 sle_1
dw_2 dw_2
pb_salir pb_salir
dw_1 dw_1
gb_1 gb_1
gb_2 gb_2
end type
global w_mant_mues_spro_cajasprod w_mant_mues_spro_cajasprod

type variables
str_mant				istr_mant

uo_Paramprecos		iuo_Paramprecos
uo_productores    iuo_productor
uo_variedades     iuo_variedad
uo_predios        iuo_predio


DataWindowChild  	idwc_pltadesp, idwc_tipdoc, idwc_numero, idwc_planta,&
                  idwc_especie, idwc_variedad, idwc_serplanta, &
						idwc_linea, idwc_predio


Long					ii_proceso, ii_lote
String				is_embalaje, is_calibre
Integer				ii_zona
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public function boolean despieceregistro (string as_registro)
public function boolean buscadatosproceso (long al_proceso)
public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente)
end prototypes

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN -1

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

This.TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN -1

IF wf_actualiza_db() THEN
		w_main.SetMicroHelp("Información Grabada.")
ELSE
		w_main.SetMicroHelp("No se puede Grabar información.")
		Message.DoubleParm = -1
		RETURN -1
END IF

RETURN 0
end event

event ue_imprimir();SetPointer(HourGlass!)

String	ls_Impresora	=	"AccuMax", &
			ls_Especie, ls_Variedad, ls_Categoria, ls_Codigo1, ls_Codigo2, &
			ls_Codigo3, ls_FDAProd, ls_Packing, ls_ComunaPack, ls_Calibre, &
			ls_Predio, ls_ProdComuna, ls_ProdProvincia, ls_Embalaje
Integer	li_Productor, li_Predio, li_Cuartel
Long		ll_Fila, ll_Trabajo
Decimal	ld_KgsNeto, ld_LbsNeto
Date		ld_FechaEmbalaje

ll_Trabajo	=	PrintOpen()
ls_Codigo1	=	"01021020010910821020020402050504"
ls_Codigo2	=	"93004600100AJJD1"

CHOOSE CASE ls_Impresora
	CASE "Zebra"
		Print(ll_Trabajo, "^XA")
		Print(ll_Trabajo, "^DFDSF^FS")
		Print(ll_Trabajo, "^PRA ")
		Print(ll_Trabajo, "^LH0,0^FS")
		Print(ll_Trabajo, "^LL280")
		Print(ll_Trabajo, "^MD15")
		Print(ll_Trabajo, "^MNY")
		Print(ll_Trabajo, "^LH0,0^FS")
		Print(ll_Trabajo, "^BY2,3.0^FO96,185^BCN,49,Y,N,N^FR^FD>:0100000010^FS")
		Print(ll_Trabajo, "^FO189,35^AGN,57,39^CI0^FR^FDBING^FS")
		Print(ll_Trabajo, "^FO129,103^ADN,17,9^CI0^FR^FDCODE^FS")
		Print(ll_Trabajo, "^FO400,108^GB111,53,3^FS")
		Print(ll_Trabajo, "^FO103,83^GB424,0,4^FS")
		Print(ll_Trabajo, "^XZ")
		Print(ll_Trabajo, "^FX End of Job")
		Print(ll_Trabajo, "^XA")
		Print(ll_Trabajo, "^XFDSF")
		Print(ll_Trabajo, "^XZ")
		
	CASE "AccuMax"
		Print(ll_Trabajo, "^@60,3")
		Print(ll_Trabajo, "^W70")
		Print(ll_Trabajo, "^H10")
		Print(ll_Trabajo, "^P1")
		Print(ll_Trabajo, "^S4")
		Print(ll_Trabajo, "^AT")
		Print(ll_Trabajo, "^C1")
		Print(ll_Trabajo, "^R0")
		Print(ll_Trabajo, "~Q+0")
		Print(ll_Trabajo, "^O0")
		Print(ll_Trabajo, "^D0")
		Print(ll_Trabajo, "^E14")
		Print(ll_Trabajo, "~R200")
		Print(ll_Trabajo, "^%")
		Print(ll_Trabajo, "")
		Print(ll_Trabajo, "Dy2-me-dd")
		Print(ll_Trabajo, "Th:m:s")
		Print(ll_Trabajo, "BQ,379,541,2,6,60,3,0," + ls_Codigo2)
		Print(ll_Trabajo, "BQ,473,360,2,5,40,3,0,000330050000000001")
		Print(ll_Trabajo, "AE,4,355,1,1,0,3," + ls_Variedad)
		Print(ll_Trabajo, "AC,48,358,1,1,0,3,"+ ls_Especie)
		Print(ll_Trabajo, "AA,76,426,1,1,0,3,Net Weight: " + &
								Trim(String(ld_KgsNeto, "0.0")) + " KN - " + &
								Trim(String(ld_LbsNeto, "##0")) + " LBS. NET")
		Print(ll_Trabajo, "AB,10,75,1,1,0,3," + ls_Categoria)
		Print(ll_Trabajo, "AD,138,96,1,1,0,0,")
		Print(ll_Trabajo, "AE,122,14,1,1,0,0,")
		Print(ll_Trabajo, "BQ,260,444,2,6,59,3,0," + ls_Codigo1)
		Print(ll_Trabajo, "Lo,48,0,48,478")
		Print(ll_Trabajo, "Lo,0,79,249,80")
		Print(ll_Trabajo, "Lo,96,0,96,478")
		Print(ll_Trabajo, "AB,59,66,1,1,0,3,BAG")
		Print(ll_Trabajo, "AA,98,396,1,1,0,3,PACKING IDENTIFICATION")
		Print(ll_Trabajo, "AA,116,478,1,1,0,3,FDA Code")
		Print(ll_Trabajo, "AA,134,478,1,1,0,3,Nombre")
		Print(ll_Trabajo, "AA,152,478,1,1,0,3,Comuna / Provincia")
		Print(ll_Trabajo, "AA,118,311,1,1,0,3," + ls_FDAProd)
		Print(ll_Trabajo, "AA,136,311,1,1,0,3," + ls_Packing)
		Print(ll_Trabajo, "AA,153,311,1,1,0,3," + ls_ComunaPack)
		Print(ll_Trabajo, "Lo,176,0,176,478")
		Print(ll_Trabajo, "AD,120,70,1,1,0,3," + ls_Calibre)
		Print(ll_Trabajo, "AA,180,398,1,1,0,3,GROWER IDENTIFICATION")
		Print(ll_Trabajo, "AA,196,478,1,1,0,3,CODE")
		Print(ll_Trabajo, "AB,194,312,1,1,0,3," + String(li_Productor, '0000') + &
								" / " + String(li_Predio, '000'))
		Print(ll_Trabajo, "AA,212,478,1,1,0,3,Predio")
		Print(ll_Trabajo, "AA,213,311,1,1,0,3," + ls_Predio + " / C" + &
								String(li_Cuartel, '001'))
		Print(ll_Trabajo, "AA,226,478,1,1,0,3,Comuna / Provincia")
		Print(ll_Trabajo, "AA,227,311,1,1,0,3," + ls_ProdComuna + " / " + &
								ls_ProdProvincia)
		Print(ll_Trabajo, "Lo,248,0,248,478")
		Print(ll_Trabajo, "Lo,221,47,222,48")
		Print(ll_Trabajo, "Lo,216,0,216,79")
		Print(ll_Trabajo, "AC,181,78,1,1,0,3," + ls_Embalaje)
		Print(ll_Trabajo, "AB,217,79,1,1,0,3," + String(ld_FechaEmbalaje, 'ddmmyy'))
		Print(ll_Trabajo, "$")
		
	CASE ELSE
		ll_Fila	=	dw_2.Retrieve(dw_1.Object.clie_codigo[1], &
											dw_1.Object.plde_codigo[1], &
											dw_1.Object.capr_numero[1])
		
		IF ll_Fila = -1 THEN
			MessageBox( "Error en Base de Datos", &
							"Se ha producido un error en Base " + &
							"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
		ELSEIF ll_Fila = 0 THEN
			MessageBox( "No Existe información", &
							"No Existe información para este informe.", &
							StopSign!, OK!)
		ELSE
			dw_2.Print()
		END IF
		
END CHOOSE

PrintClose(ll_Trabajo)

SetPointer(Arrow!)
end event

event ue_antesguardar();Integer 	il_fila, il_cont
Long		ll_fila
String	ls_mensaje, ls_columna, ls_fecha

dw_1.accepttext()

//IF Isnull(dw_1.GetItemNumber(1,"ccpr_folio")) or &
//	String(dw_1.GetItemNumber(1,"ccpr_folio")) = "" THEN
//	ls_mensaje = "Número de Folio"
//	il_cont++
//END IF
//
//IF Isnull(dw_1.GetItemNumber(1,"espe_codigo")) or&
//	String(dw_1.GetItemNumber(1,"espe_codigo")) = "" THEN
//	ls_mensaje = "Especie "
//	il_cont++
//END IF
//
///* Se rescata la fecha desde la datawindows para luego ver si es que*/
//					/* es nula o fue mal ingresada*/
//
//ls_fecha = string(dw_1.Object.ccpr_fechai[1])
//
//IF Isnull(ls_fecha) or (ls_fecha) = "00/00/0000" THEN
//	ls_mensaje = "Fecha"
//	il_cont++
//END IF
//
//IF Isnull(dw_1.GetItemNumber(1,"vari_codigo")) or&
//	String(dw_1.GetItemNumber(1,"vari_codigo")) = "" THEN
//	ls_mensaje = "Variedad "
//	il_cont++
//END IF
//
//IF Isnull(dw_1.GetItemNumber(1,"ccpr_diadpf")) or&
//	String(dw_1.GetItemNumber(1,"ccpr_diadpf")) = "" THEN
//	ls_mensaje = "DDFF "
//	il_cont++
//END IF
//
//IF il_cont  = 1 THEN
//	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
//	dw_1.SetFocus()
//	Message.DoubleParm = -1
//	RETURN 
//ELSEIF il_cont > 1 THEN
//	ls_mensaje = "Datos"
//	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
//	dw_1.SetFocus()
//	Message.DoubleParm = -1
//	RETURN 
//END IF
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

public function boolean despieceregistro (string as_registro);Integer		li_lectura, li_sigte=1, li_larg, li_lectura2
String		ls_caracter, ls_compone, ls_compone2

FOR li_lectura=1 TO Len(as_registro)
	ls_caracter = Mid(as_registro,li_lectura,1)
	ls_compone=''
   IF ls_caracter = '&' THEN
			ls_compone = Mid(as_registro,li_lectura + 1,2)
		   li_sigte=li_lectura + 3
			li_larg=0
			
			li_lectura2=li_sigte
			DO WHILE Mid(as_registro,li_sigte,1) <> "&" AND li_sigte <= Len(as_registro)
				li_sigte++
				li_larg++
			LOOP		 
         ls_compone2 = Mid(as_registro,li_lectura2,li_larg)
	
		   CHOOSE CASE ls_compone
	
				 CASE "01"
					ii_proceso 	= 	Long(ls_compone2)	
				 CASE "02"
					ii_lote    	= 	Long(ls_compone2)	
				 CASE "03"
					is_embalaje =	ls_compone2
					dw_1.Object.emba_codigo[1]	=	is_embalaje
				 CASE "04"			
					is_calibre	=  ls_compone2
					dw_1.Object.capr_calibr[1]	=	is_calibre
			END CHOOSE
			
			li_lectura = li_sigte - 1
	END IF

NEXT

RETURN TRUE

end function

public function boolean buscadatosproceso (long al_proceso);Integer		li_Cliente, li_planta, li_especie, li_variedad
Date			ld_fecha
Long			ll_productor

SELECT clie_codigo,plde_codigo,orpr_fecpro,prod_codigo,espe_codigo,vari_codigo
INTO :li_cliente,:li_planta,:ld_fecha,:ll_productor,:li_especie,:li_variedad
FROM dba.spro_ordenproceso
WHERE orpr_numero=:al_proceso;

dw_1.Object.clie_codigo[1]	=	li_cliente
dw_1.Object.plde_codigo[1]	=	li_planta
dw_1.Object.capr_fecemb[1]	=	ld_fecha
dw_1.Object.prod_codigo[1]	=	ll_productor
dw_1.Object.espe_codigo[1]	=	li_especie
dw_1.Object.vari_codigo[1]	=	li_variedad

SELECT zona_codigo
INTO	:ii_zona
FROM dba.plantadesp
WHERE plde_codigo = :li_planta;


RETURN TRUE
end function

public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente);String	ls_numero


SELECT max(capr_numero)
INTO   :ls_numero
FROM dba.spro_cajasprod
WHERE clie_codigo	=	:ai_cliente
AND   plde_codigo =  :ai_bodega;

IF IsNull(ls_numero) or ls_numero='' THEN
	ls_numero = String(ii_zona,'0000')+String(ai_bodega,'0000')+'0000000001' 
ELSE
	ls_numero = Mid(ls_numero,1,8)+ String( Long(Mid(ls_numero,9,10))+1,'0000000000' )
END IF

dw_1.Object.capr_numero[1]	=	ls_numero

RETURN True
end function

on w_mant_mues_spro_cajasprod.create
this.pb_ok=create pb_ok
this.sle_1=create sle_1
this.dw_2=create dw_2
this.pb_salir=create pb_salir
this.dw_1=create dw_1
this.gb_1=create gb_1
this.gb_2=create gb_2
this.Control[]={this.pb_ok,&
this.sle_1,&
this.dw_2,&
this.pb_salir,&
this.dw_1,&
this.gb_1,&
this.gb_2}
end on

on w_mant_mues_spro_cajasprod.destroy
destroy(this.pb_ok)
destroy(this.sle_1)
destroy(this.dw_2)
destroy(this.pb_salir)
destroy(this.dw_1)
destroy(this.gb_1)
destroy(this.gb_2)
end on

event open;Long ll_fila

x=0
y=0

iuo_ParamPrecos	=	Create uo_Paramprecos
iuo_productor		=	Create uo_productores    
iuo_variedad      =  Create uo_variedades     
iuo_predio        =  Create uo_predios        

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

sle_1.Text	=	Char(13)
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

type pb_ok from picturebutton within w_mant_mues_spro_cajasprod
event ue_mousemove pbm_mousemove
integer x = 3269
integer y = 504
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo\Bmp\ACEPTAE.BMP"
alignment htextalign = left!
end type

event clicked;String	ls_Registro

dw_1.AcceptText()

IF Isnull(dw_1.Object.capr_regcap[1]) or &
	dw_1.Object.capr_regcap[1] = "" or &
	dw_1.Object.capr_regcap[1] = "-1" THEN
	Messagebox('ERROR','CAPTURA ERRONEA')
ELSE
	ls_Registro	=	dw_1.Object.capr_regcap[1]
	
	IF despieceregistro(ls_Registro) = True THEN
//				Parent.triggerevent("ue_recuperadatos")
	END IF
	
	IF buscadatosproceso(ii_proceso) = True THEN
//				Parent.triggerevent("ue_recuperadatos")
	END IF
	
	IF buscanuevocorrelativo(Integer(dw_1.Object.plde_codigo[1]),Integer(dw_1.Object.clie_codigo[1])) = True THEN
//				Parent.triggerevent("ue_recuperadatos")
	END IF
	
	Parent.TriggerEvent("ue_guardar")
	
	Parent.TriggerEvent("ue_imprimir")
	
	dw_1.Reset()
	dw_1.InsertRow(0)
	dw_1.SetFocus()
	dw_1.SetColumn("capr_regcap")
END IF
end event

type sle_1 from singlelineedit within w_mant_mues_spro_cajasprod
integer x = 763
integer y = 1352
integer width = 402
integer height = 112
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_mant_mues_spro_cajasprod
boolean visible = false
integer x = 23
integer y = 1364
integer width = 686
integer height = 400
integer taborder = 20
string title = "none"
string dataobject = "dw_info_spro_cajasprod"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type pb_salir from picturebutton within w_mant_mues_spro_cajasprod
event ue_mousemove pbm_mousemove
integer x = 3269
integer y = 1112
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

type dw_1 from datawindow within w_mant_mues_spro_cajasprod
integer x = 32
integer y = 36
integer width = 3150
integer height = 1248
integer taborder = 10
string dataobject = "dw_mant_mues_spro_cajasprod"
borderstyle borderstyle = stylelowered!
end type

event itemchanged;//Integer	planta, numerofol,li_null
//String	ls_columna
//SetNull(li_Null)
//
//dw_1.accepttext()
//
////planta 		= dw_1.Object.plde_codigo[dw_1.getrow()]
////numerofol 	= dw_1.Object.ccpr_folio[dw_1.getrow()]
//ls_columna  = GetColumnName()
//
//CHOOSE CASE ls_columna
//	
//	CASE "capr_regcap"
//
//		IF Isnull(dw_1.Object.capr_regcap[1]) or &
//			dw_1.Object.capr_regcap[1] = "" or &
//			dw_1.Object.capr_regcap[1] = "-1" THEN
//			Messagebox('ERROR','CAPTURA ERRONEA')
//		ELSE
//			
//			IF despieceregistro(data) = True THEN
////				Parent.triggerevent("ue_recuperadatos")
//			END IF
//			
//			IF buscadatosproceso(ii_proceso) = True THEN
////				Parent.triggerevent("ue_recuperadatos")
//			END IF
//			
//			IF buscanuevocorrelativo(Integer(dw_1.Object.plde_codigo[1]),Integer(dw_1.Object.clie_codigo[1])) = True THEN
////				Parent.triggerevent("ue_recuperadatos")
//			END IF
//			
//			Parent.TriggerEvent("ue_guardar")
//			
//			//Parent.TriggerEvent("ue_imprimir")
//			
//			dw_1.Reset()
//			dw_1.InsertRow(0)
//			
//			return 1//dw_1.SetColumn("capr_regcap")
//			
//		END IF
//END CHOOSE
//
end event

event itemerror;Return 1
end event

type gb_1 from groupbox within w_mant_mues_spro_cajasprod
integer x = 3214
integer y = 1044
integer width = 247
integer height = 244
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_2 from groupbox within w_mant_mues_spro_cajasprod
integer x = 3214
integer y = 436
integer width = 247
integer height = 244
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

