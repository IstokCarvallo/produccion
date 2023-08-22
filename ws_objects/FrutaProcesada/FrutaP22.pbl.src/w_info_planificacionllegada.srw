$PBExportHeader$w_info_planificacionllegada.srw
forward
global type w_info_planificacionllegada from w_para_informes
end type
type rb_actual from radiobutton within w_info_planificacionllegada
end type
type rb_dia from radiobutton within w_info_planificacionllegada
end type
type gb_3 from groupbox within w_info_planificacionllegada
end type
type st_4 from statictext within w_info_planificacionllegada
end type
end forward

global type w_info_planificacionllegada from w_para_informes
integer x = 14
integer y = 32
integer width = 2505
integer height = 1352
string title = "Informes planificación llegada desde packing"
boolean minbox = false
string icon = "F:\Desarrollo\Productiva\FrutaProcesada\Informes.ico"
rb_actual rb_actual
rb_dia rb_dia
gb_3 gb_3
st_4 st_4
end type
global w_info_planificacionllegada w_info_planificacionllegada

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

on w_info_planificacionllegada.create
int iCurrent
call super::create
this.rb_actual=create rb_actual
this.rb_dia=create rb_dia
this.gb_3=create gb_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_actual
this.Control[iCurrent+2]=this.rb_dia
this.Control[iCurrent+3]=this.gb_3
this.Control[iCurrent+4]=this.st_4
end on

on w_info_planificacionllegada.destroy
call super::destroy
destroy(this.rb_actual)
destroy(this.rb_dia)
destroy(this.gb_3)
destroy(this.st_4)
end on

event open;


istr_mant = Message.PowerObjectParm





end event

type pb_excel from w_para_informes`pb_excel within w_info_planificacionllegada
end type

type st_computador from w_para_informes`st_computador within w_info_planificacionllegada
end type

type st_usuario from w_para_informes`st_usuario within w_info_planificacionllegada
end type

type st_temporada from w_para_informes`st_temporada within w_info_planificacionllegada
end type

type p_logo from w_para_informes`p_logo within w_info_planificacionllegada
end type

type st_titulo from w_para_informes`st_titulo within w_info_planificacionllegada
integer width = 1751
string text = "Informe Planificación Llegada desde Packing"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_planificacionllegada
integer x = 2130
integer y = 480
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		fila
Integer	li_packing
str_info	lstr_info

lstr_info.titulo	= "PLANIFICACION LLEGADA DESDE PACKING"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

IF rb_actual.Checked THEN
	vinf.dw_1.DataObject = "dw_info_planificacionllegada"
	li_packing = Integer(istr_mant.argumento[3])
ELSE
	vinf.dw_1.DataObject = "dw_info_planificacionllegada_dia"
	li_packing = -1
END IF	

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									li_packing,date(istr_mant.argumento[4]),Time(istr_mant.argumento[5]))

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
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_planificacionllegada
integer x = 2126
integer y = 748
integer taborder = 60
end type

type rb_actual from radiobutton within w_info_planificacionllegada
integer x = 594
integer y = 600
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
long backcolor = 553648127
string text = "Actual"
boolean checked = true
end type

type rb_dia from radiobutton within w_info_planificacionllegada
integer x = 594
integer y = 708
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
long backcolor = 553648127
string text = "Día"
end type

type gb_3 from groupbox within w_info_planificacionllegada
integer x = 439
integer y = 540
integer width = 1477
integer height = 280
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_4 from statictext within w_info_planificacionllegada
integer x = 251
integer y = 440
integer width = 1751
integer height = 520
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33543637
long backcolor = 33543637
boolean enabled = false
alignment alignment = center!
boolean border = true
long bordercolor = 12632256
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

