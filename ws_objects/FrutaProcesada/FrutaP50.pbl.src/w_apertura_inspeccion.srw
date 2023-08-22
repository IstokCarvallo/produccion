$PBExportHeader$w_apertura_inspeccion.srw
$PBExportComments$Emisión Solicitud de Inspección.
forward
global type w_apertura_inspeccion from w_para_informes
end type
type st_1 from statictext within w_apertura_inspeccion
end type
type dw_cliente from datawindow within w_apertura_inspeccion
end type
type st_2 from statictext within w_apertura_inspeccion
end type
type dw_plantadesp from datawindow within w_apertura_inspeccion
end type
type st_4 from statictext within w_apertura_inspeccion
end type
type ddlb_tipocond from dropdownlistbox within w_apertura_inspeccion
end type
type em_numero from editmask within w_apertura_inspeccion
end type
type st_3 from statictext within w_apertura_inspeccion
end type
type st_5 from statictext within w_apertura_inspeccion
end type
type cbx_terceros from checkbox within w_apertura_inspeccion
end type
type st_6 from statictext within w_apertura_inspeccion
end type
type pb_grabar from picturebutton within w_apertura_inspeccion
end type
type dw_alpalletencab from datawindow within w_apertura_inspeccion
end type
type dw_alpalletfruta from datawindow within w_apertura_inspeccion
end type
type dw_palletfrutahisto from datawindow within w_apertura_inspeccion
end type
type dw_palletencabhisto from datawindow within w_apertura_inspeccion
end type
end forward

global type w_apertura_inspeccion from w_para_informes
integer width = 3410
integer height = 1644
string title = "APERTURA DE INSPECCION"
boolean minbox = false
boolean maxbox = false
event ue_validapassword ( )
st_1 st_1
dw_cliente dw_cliente
st_2 st_2
dw_plantadesp dw_plantadesp
st_4 st_4
ddlb_tipocond ddlb_tipocond
em_numero em_numero
st_3 st_3
st_5 st_5
cbx_terceros cbx_terceros
st_6 st_6
pb_grabar pb_grabar
dw_alpalletencab dw_alpalletencab
dw_alpalletfruta dw_alpalletfruta
dw_palletfrutahisto dw_palletfrutahisto
dw_palletencabhisto dw_palletencabhisto
end type
global w_apertura_inspeccion w_apertura_inspeccion

type variables
Str_mant				istr_mant
DataWindowChild	dwc_plantas
Integer	ii_planta, ii_cliente, ii_condicion
Long		il_numero, il_proceso

end variables

forward prototypes
public function boolean wf_actualiza_db (boolean borrando)
end prototypes

event ue_validapassword();Str_mant					lstr_mant

lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_Password+''+'contraparte'

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_alpalletfruta.Update(True, False) = 1 THEN
	IF dw_alpalletencab.Update(True, False) = 1 THEN
		IF dw_palletfrutahisto.Update(True, False) = 1 THEN
			IF dw_palletencabhisto.Update(True, False) = 1 THEN
				Commit;
				 
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				ELSE
					lb_Retorno	=	True
					
					dw_alpalletfruta.ResetUpdate()
					dw_alpalletencab.ResetUpdate()
					dw_palletfrutahisto.ResetUpdate()
					dw_palletencabhisto.ResetUpdate()							
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
				RETURN lb_Retorno
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
			RETURN lb_Retorno
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
		RETURN lb_Retorno
	END IF
ELSE
	F_ErrorBaseDatos(sqlca, This.Title)
	RollBack;
	RETURN lb_Retorno
END IF
	
Update dbo.inspecpalenc Set
inpe_estado = null,
inpe_proces = null
Where :ii_cliente in (-1,clie_codigo)
And plde_codigo = :ii_planta
And inpe_tipoin = :ii_condicion
And inpe_numero = :il_numero;
	
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_apertura_inspeccion.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.dw_plantadesp=create dw_plantadesp
this.st_4=create st_4
this.ddlb_tipocond=create ddlb_tipocond
this.em_numero=create em_numero
this.st_3=create st_3
this.st_5=create st_5
this.cbx_terceros=create cbx_terceros
this.st_6=create st_6
this.pb_grabar=create pb_grabar
this.dw_alpalletencab=create dw_alpalletencab
this.dw_alpalletfruta=create dw_alpalletfruta
this.dw_palletfrutahisto=create dw_palletfrutahisto
this.dw_palletencabhisto=create dw_palletencabhisto
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_plantadesp
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.ddlb_tipocond
this.Control[iCurrent+7]=this.em_numero
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.cbx_terceros
this.Control[iCurrent+11]=this.st_6
this.Control[iCurrent+12]=this.pb_grabar
this.Control[iCurrent+13]=this.dw_alpalletencab
this.Control[iCurrent+14]=this.dw_alpalletfruta
this.Control[iCurrent+15]=this.dw_palletfrutahisto
this.Control[iCurrent+16]=this.dw_palletencabhisto
end on

on w_apertura_inspeccion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.dw_plantadesp)
destroy(this.st_4)
destroy(this.ddlb_tipocond)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.cbx_terceros)
destroy(this.st_6)
destroy(this.pb_grabar)
destroy(this.dw_alpalletencab)
destroy(this.dw_alpalletfruta)
destroy(this.dw_palletfrutahisto)
destroy(this.dw_palletencabhisto)
end on

event open;call super::open;dw_cliente.SetTransObject(Sqlca)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_plantadesp.GetChild("plde_codigo", dwc_plantas)
dwc_plantas.SetTransObject(Sqlca)
dwc_plantas.Retrieve(1)			//Plantas de Despacho

dw_plantadesp.SetTransObject(Sqlca)
dw_plantadesp.InsertRow(0)
dw_plantadesp.SetItem(1,"plde_codigo", gi_CodPlanta)

ddlb_tipocond.SelectItem(1)

istr_mant.Argumento[1]	=	String(gi_CodExport)
istr_mant.Argumento[5]	=	String(gi_CodPlanta)
istr_mant.Argumento[3]	=	"1"

PostEvent("ue_validapassword")

dw_alpalletencab.SettransObject(Sqlca)
dw_alpalletfruta.SettransObject(Sqlca)
dw_palletencabhisto.SettransObject(Sqlca)
dw_palletfrutahisto.SettransObject(Sqlca)
end event

type pb_excel from w_para_informes`pb_excel within w_apertura_inspeccion
end type

type st_computador from w_para_informes`st_computador within w_apertura_inspeccion
end type

type st_usuario from w_para_informes`st_usuario within w_apertura_inspeccion
end type

type st_temporada from w_para_informes`st_temporada within w_apertura_inspeccion
end type

type p_logo from w_para_informes`p_logo within w_apertura_inspeccion
end type

type st_titulo from w_para_informes`st_titulo within w_apertura_inspeccion
integer width = 1989
string text = "Apertura de Inspección"
end type

type pb_acepta from w_para_informes`pb_acepta within w_apertura_inspeccion
boolean visible = false
integer x = 2761
integer y = 720
integer taborder = 50
end type

type pb_salir from w_para_informes`pb_salir within w_apertura_inspeccion
integer x = 2377
integer y = 956
integer taborder = 60
end type

type st_1 from statictext within w_apertura_inspeccion
integer x = 338
integer y = 492
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_apertura_inspeccion
integer x = 686
integer y = 492
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1] = data
	dwc_plantas.Retrieve(1)	
ELSE
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_apertura_inspeccion
integer x = 338
integer y = 628
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_apertura_inspeccion
integer x = 686
integer y = 628
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExistePlanta(Integer(istr_mant.Argumento[1]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[5] = data
ELSE
	RETURN 1
END IF
end event

type st_4 from statictext within w_apertura_inspeccion
integer x = 338
integer y = 764
integer width = 334
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Condición"
boolean focusrectangle = false
end type

type ddlb_tipocond from dropdownlistbox within w_apertura_inspeccion
integer x = 686
integer y = 764
integer width = 526
integer height = 292
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
string text = "none"
boolean sorted = false
string item[] = {"Inspección","Re-Inspección"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;istr_mant.argumento[3]	=	String(index)
end event

type em_numero from editmask within w_apertura_inspeccion
integer x = 686
integer y = 904
integer width = 402
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[4]	=	This.Text

dw_alpalletencab.Reset()
dw_alpalletfruta.Reset()
dw_palletencabhisto.Reset()
dw_palletfrutahisto.Reset()

dw_alpalletencab.SettransObject(Sqlca)
dw_alpalletfruta.SettransObject(Sqlca)
dw_palletencabhisto.SettransObject(Sqlca)
dw_palletfrutahisto.SettransObject(Sqlca)
end event

type st_3 from statictext within w_apertura_inspeccion
integer x = 338
integer y = 904
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Número"
boolean focusrectangle = false
end type

type st_5 from statictext within w_apertura_inspeccion
integer x = 247
integer y = 440
integer width = 1989
integer height = 624
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_terceros from checkbox within w_apertura_inspeccion
integer x = 686
integer y = 1080
integer width = 1253
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Incluye Terceros mismas Características"
end type

event clicked;IF This.Checked THEN
	dw_cliente.Enabled		=	False
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[1]	=	'-1'
ELSE
	dw_cliente.Enabled		=	True
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	istr_mant.argumento[1]	=	String(dw_cliente.Object.clie_codigo[1])
END IF
end event

type st_6 from statictext within w_apertura_inspeccion
integer x = 247
integer y = 1064
integer width = 1989
integer height = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_grabar from picturebutton within w_apertura_inspeccion
integer x = 2377
integer y = 664
integer width = 302
integer height = 244
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = right!
end type

event clicked;SetPointer(HourGlass!)

Long		ll_Filas, ll_proceso, ll_numero,ll_filas1,ll_filas2,ll_filas3
Integer	li_varrot, li_Cliente, li_planta, li_condicion

IF cbx_terceros.checked THEN
	ii_Cliente = -1
ELSE
	ii_Cliente = dw_cliente.Object.clie_codigo[1]
END IF

ii_condicion = Integer(istr_mant.Argumento[3])
il_numero 	= Long(istr_mant.Argumento[4])
ii_planta	= Integer(istr_mant.Argumento[5])


//dw_alpalletencab.Reset()
//dw_alpalletfruta.Reset()
//dw_palletencabhisto.Reset()
//dw_palletfrutahisto.Reset()
//
//dw_alpalletencab.SettransObject(Sqlca)
//dw_alpalletfruta.SettransObject(Sqlca)
//dw_palletencabhisto.SettransObject(Sqlca)
//dw_palletfrutahisto.SettransObject(Sqlca)
//
//IF em_numero.Text = "" THEN RETURN
//
//IF cbx_terceros.checked THEN
//	ii_Cliente = -1
//ELSE
//	ii_Cliente = dw_cliente.Object.clie_codigo[1]
//END IF
//
//ii_planta = Integer(istr_mant.Argumento[5])
//ii_condicion = Integer(istr_mant.Argumento[3])
//il_numero = Long(istr_mant.Argumento[4])
//
//Select distinct inpe_proces
//Into :il_proceso
//From dbo.inspecpalenc
//Where :ii_cliente in (-1,clie_codigo)
//And plde_codigo = :ii_planta
//And inpe_tipoin = :ii_condicion
//And inpe_numero = :il_numero;
//
//ll_filas = dw_alpalletencab.Retrieve(ii_cliente,ii_planta,il_proceso)
//
//IF ll_filas = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF ll_filas = 0 THEN
//	MessageBox( "No Existe información", "No existe información para eliminar.", &
//	StopSign!, Ok!)
//ELSE
////	dw_alpalletencab.Retrieve(ii_cliente,ii_planta,il_proceso)
////	dw_alpalletfruta.Retrieve(ii_cliente,ii_planta,il_proceso)
////	dw_palletfrutahisto.Retrieve(ii_cliente,ii_planta,il_proceso)
////	dw_palletencabhisto.Retrieve(ii_cliente,ii_planta,il_proceso)
////	
////	dw_alpalletfruta.RowsMove(1,dw_alpalletfruta.RowCount(),Primary!,dw_alpalletfruta,1,Delete!)
////	dw_alpalletencab.RowsMove(1,dw_alpalletencab.RowCount(),Primary!,dw_alpalletencab,1,Delete!)
////	dw_palletfrutahisto.RowsMove(1,dw_palletfrutahisto.RowCount(),Primary!,dw_palletfrutahisto,1,Delete!)
////	dw_palletencabhisto.RowsMove(1,dw_palletencabhisto.RowCount(),Primary!,dw_palletencabhisto,1,Delete!)
//		
//	w_main.SetMicroHelp("Borrando Registro...")
//	IF wf_actualiza_db(True) THEN
//		w_main.SetMicroHelp("Registro Borrado...")
//		MessageBox('Atención',"Proceso Realizado Satisfactoriamente.")
//		SetPointer(Arrow!)
//	ELSE
//		w_main.SetMicroHelp("Registro no Borrado...")
//	END IF			
//				
//END IF

UPDATE dbo.inspecpalenc SET
	inpe_estado = 5,
	inpe_proces = null
WHERE :ii_cliente IN (-1,clie_codigo)
AND plde_codigo = :ii_planta
AND inpe_tipoin = :ii_condicion
AND inpe_numero = :il_numero
AND inpe_estado in (1,2);
COMMIT;

UPDATE dbo.palletencab SET
	paen_inspec = 5
WHERE :ii_cliente IN (-1,clie_codigo)
AND plde_codigo = :ii_planta
AND inpe_tipoin = :ii_condicion
AND inpe_numero = :il_numero
AND paen_inspec = 1;
COMMIT;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla INSPECPALENC")
	em_numero.SetFocus()
	
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe Nro. Inspección Indicado.~r~rIngrese otro Número.", &
					Exclamation!, Ok!)
	em_numero.SetFocus()
ELSE
	MessageBox('Atención',"Proceso Realizado Satisfactoriamente.")
END IF



end event

type dw_alpalletencab from datawindow within w_apertura_inspeccion
boolean visible = false
integer x = 50
integer y = 1244
integer width = 261
integer height = 216
integer taborder = 70
boolean bringtotop = true
string title = "alpalletencab"
string dataobject = "dw_alpalletencab_his"
borderstyle borderstyle = stylelowered!
end type

type dw_alpalletfruta from datawindow within w_apertura_inspeccion
boolean visible = false
integer x = 329
integer y = 1240
integer width = 261
integer height = 216
integer taborder = 80
boolean bringtotop = true
string title = "alpalletfruta"
string dataobject = "dw_alpalletfruta_inpe"
borderstyle borderstyle = stylelowered!
end type

type dw_palletfrutahisto from datawindow within w_apertura_inspeccion
boolean visible = false
integer x = 599
integer y = 1228
integer width = 261
integer height = 216
integer taborder = 90
boolean bringtotop = true
string title = "palletfrutahisto"
string dataobject = "dw_borra_palletfrutahisto"
borderstyle borderstyle = stylelowered!
end type

type dw_palletencabhisto from datawindow within w_apertura_inspeccion
boolean visible = false
integer x = 869
integer y = 1232
integer width = 261
integer height = 216
integer taborder = 100
boolean bringtotop = true
string title = "palletencabhisto"
string dataobject = "dw_borra_palletencabhisto"
borderstyle borderstyle = stylelowered!
end type

