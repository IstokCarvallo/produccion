$PBExportHeader$w_info_detallestandarcomponente.srw
forward
global type w_info_detallestandarcomponente from w_para_informes
end type
type rb_todos from radiobutton within w_info_detallestandarcomponente
end type
type rb_sin_deta from radiobutton within w_info_detallestandarcomponente
end type
type cbx_guias from checkbox within w_info_detallestandarcomponente
end type
type gb_3 from groupbox within w_info_detallestandarcomponente
end type
type st_4 from statictext within w_info_detallestandarcomponente
end type
end forward

global type w_info_detallestandarcomponente from w_para_informes
integer x = 14
integer y = 32
integer width = 2514
integer height = 1220
string title = "Informes Detalle de Recepciones"
boolean minbox = false
string icon = "F:\Desarrollo\Productiva\FrutaProcesada\Informes.ico"
rb_todos rb_todos
rb_sin_deta rb_sin_deta
cbx_guias cbx_guias
gb_3 gb_3
st_4 st_4
end type
global w_info_detallestandarcomponente w_info_detallestandarcomponente

type variables
Integer codigo
String  nombre

str_mant istr_mant

end variables

forward prototypes
public function boolean existeoperacion (long li_operacion)
public function string buscdescfruta (integer fruta)
end prototypes

public function boolean existeoperacion (long li_operacion);//Date		ld_fzarpe
//integer  li_cliente
//
//IF li_operacion > 0 THEN
//	SELECT	DISTINCT  emb.clie_codigo
//		INTO  :li_cliente
//		FROM	dba.embarqueprod as emb,dba.recibidores as rec 
//		WHERE	emb.oper_codigo = :li_operacion and
//				emb.reci_codigo = rec.reci_codigo and
//				emb.clie_codigo = :gi_codexport;
//				
//	IF sqlca.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla OPERACION")
//		em_1.SetFocus()
//		
//		RETURN False
//	ELSEIF sqlca.SQLCode = 100 THEN
//		MessageBox("Atención", "No existe Operación Indicada.~r~rIngrese otro Número.", &
//						Exclamation!, Ok!)
//		
//		em_1.SetFocus()
//		RETURN False
//	ELSE
//		idwc_recibidor_operacion.Reset()
//		idwc_recibidor_operacion.SetTransObject(sqlca)
//		idwc_recibidor_operacion.Retrieve(li_operacion)
//		
//	RETURN True
//	END IF
//ELSE
//	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
//					Exclamation!, Ok!)
	RETURN False
//END IF

end function

public function string buscdescfruta (integer fruta);Integer	li_codigo
String	ls_descri, ls_abrevi

SELECT frre_codigo,frre_descri,frre_abrevi  
INTO :li_codigo,:ls_descri,:ls_abrevi  
FROM dba.frutarecibida  
WHERE frre_codigo = :fruta;

		
RETURN ls_descri

end function

on w_info_detallestandarcomponente.create
int iCurrent
call super::create
this.rb_todos=create rb_todos
this.rb_sin_deta=create rb_sin_deta
this.cbx_guias=create cbx_guias
this.gb_3=create gb_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_todos
this.Control[iCurrent+2]=this.rb_sin_deta
this.Control[iCurrent+3]=this.cbx_guias
this.Control[iCurrent+4]=this.gb_3
this.Control[iCurrent+5]=this.st_4
end on

on w_info_detallestandarcomponente.destroy
call super::destroy
destroy(this.rb_todos)
destroy(this.rb_sin_deta)
destroy(this.cbx_guias)
destroy(this.gb_3)
destroy(this.st_4)
end on

event open;//em_1.SetFocus()
//dw_recibidor_operacion.SetTransObject(sqlca)
//dw_recibidor_operacion.GetChild("reci_codigo", idwc_recibidor_operacion)
//idwc_recibidor_operacion.SetTransObject(sqlca)
//idwc_recibidor_operacion.Retrieve(-1) 
//dw_recibidor_operacion.InsertRow(1)
//
//Codigo			=	-1
//Nombre			=	'Todas'
//iuo_recibidor  =	Create uo_recibidores


istr_mant = Message.PowerObjectParm





end event

type pb_excel from w_para_informes`pb_excel within w_info_detallestandarcomponente
end type

type st_computador from w_para_informes`st_computador within w_info_detallestandarcomponente
end type

type st_usuario from w_para_informes`st_usuario within w_info_detallestandarcomponente
end type

type st_temporada from w_para_informes`st_temporada within w_info_detallestandarcomponente
end type

type p_logo from w_para_informes`p_logo within w_info_detallestandarcomponente
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_detallestandarcomponente
integer width = 1751
string text = "Informe Recepción"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_detallestandarcomponente
integer x = 2203
integer y = 400
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		fila, ll_Guia, ll_tempor
Date		ld_desde, ld_hasta
String   ls_recepcion, ls_descri

str_info	lstr_info

SELECT MAX(pate_tempor)
INTO   :ll_tempor
FROM dba.paramtemporada;

SELECT pate_inicio,pate_termin
INTO   :ld_desde,:ld_hasta
FROM dba.paramtemporada
WHERE pate_tempor=:ll_tempor;

ll_Guia	=	Long(istr_mant.argumento[2])
	
IF rb_todos.Checked THEN
	lstr_info.titulo	= "RECEPCION DE PALLETS"
	lstr_info.copias	= 1
	
	OpenWithParm(vinf,lstr_info)
	
	vinf.dw_1.DataObject = "dw_info_recepcion_pallet"
END IF	
IF rb_sin_deta.Checked THEN
	lstr_info.titulo	= "RECEPCION DE PALLETS SIN DETALLE"
	lstr_info.copias	= 1	
	OpenWithParm(vinf,lstr_info)
	vinf.dw_1.DataObject = "dw_info_recepcion_pallet_sindetalle"	
END IF

IF cbx_guias.Checked THEN
	ll_Guia = -1
	lstr_info.titulo	= "RECEPCIONES CON PALLETS SIN DETALLE"
	lstr_info.copias	= 1	
	
	OpenWithParm(vinf,lstr_info)
	vinf.dw_1.DataObject = "dw_info_recepcion_palletsindetalletodos"	
END IF
		
	vinf.dw_1.SetTransObject(sqlca)
	
	//istr_mant.argumento[30] = String(dw_2.Object.rfpe_numero[1])
	
	ls_descri = buscdescfruta(Integer(istr_mant.argumento[38]))
	
	ls_recepcion = "Recepcion " + istr_mant.argumento[2]
	ls_recepcion = ls_recepcion + "                       Fecha "+istr_mant.argumento[41]
	ls_recepcion = ls_recepcion + "                       Guia "+istr_mant.argumento[30]
	ls_recepcion = ls_recepcion + "                       En Planta "+istr_mant.argumento[1]
	
	//fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]), &
	//ld_desde,ld_hasta,0,Long(istr_mant.argumento[2]),0,0)
	fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]), &
	ld_desde,ld_hasta,0,ll_Guia,0,0)

	
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		IF gs_Ambiente <> 'Windows' THEN
			F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		END IF
		vinf.dw_1.Modify("guia.text = '" + ls_recepcion + "'")
		vinf.dw_1.Modify("fruta.text = '" + ls_descri + "'")  
		
		vinf.dw_1.Object.DataWindow.Zoom = 95
		
		vinf.Visible	= True
		vinf.Enabled	= True
	END IF

SetPointer(Arrow!)
	
	
	
	
	
	
	
	
	//SetPointer(Arrow!)
	//
	//Integer	fila, li_operacion,li_recibidor
	//	
	//li_operacion=integer(em_1.text)	
	//	
	//IF cbx_recibidor.checked THEN
	//	li_recibidor = -1 
	//ELSE                
	//	li_recibidor =dw_recibidor_operacion.Object.reci_codigo[1]
	//END IF
	//
	//istr_info.titulo	= 'Informe Etiquetados Especiales '
	//
	//OpenWithParm(vinf, istr_info)
	//vinf.dw_1.DataObject = "dw_info_operacionrecibidor"
	//vinf.dw_1.SetTransObject(sqlca)
	//
	//
	//
	//fila = vinf.dw_1.Retrieve(li_operacion,li_recibidor)
	//
	//IF fila = -1 THEN
	//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
	//			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	//
	//ELSEIF fila = 0 THEN
	//	MessageBox( "No Existe información", "No existe información para este informe.", &
	//	             StopSign!, Ok!)
	//
	// ELSE
	//		F_Membrete(vinf.dw_1)
	//		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	//		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	//		vinf.Visible	= True
	//		vinf.Enabled	= True
	//END IF
//SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_detallestandarcomponente
integer x = 2199
integer y = 668
integer taborder = 60
end type

type rb_todos from radiobutton within w_info_detallestandarcomponente
integer x = 585
integer y = 532
integer width = 1097
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Pallets Recepcionados Con Detalle"
boolean checked = true
end type

event clicked;IF rb_todos.Checked THEN
	cbx_guias.Enabled	= False
	cbx_guias.Checked	= False
END IF
end event

type rb_sin_deta from radiobutton within w_info_detallestandarcomponente
integer x = 585
integer y = 640
integer width = 1093
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Pallets Recepcionados Sin Detalle"
end type

event clicked;IF rb_sin_deta.Checked THEN
	cbx_guias.Enabled	= True
END IF
end event

type cbx_guias from checkbox within w_info_detallestandarcomponente
integer x = 585
integer y = 780
integer width = 539
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Todas las Guías"
end type

type gb_3 from groupbox within w_info_detallestandarcomponente
integer x = 430
integer y = 472
integer width = 1477
integer height = 280
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_4 from statictext within w_info_detallestandarcomponente
integer x = 251
integer y = 440
integer width = 1751
integer height = 520
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
alignment alignment = center!
boolean border = true
boolean focusrectangle = false
end type

