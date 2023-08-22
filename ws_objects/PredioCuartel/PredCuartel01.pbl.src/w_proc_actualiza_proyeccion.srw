$PBExportHeader$w_proc_actualiza_proyeccion.srw
$PBExportComments$Informe de Facturación Productor Mensual.
forward
global type w_proc_actualiza_proyeccion from w_para_informes
end type
type st_1 from statictext within w_proc_actualiza_proyeccion
end type
type st_2 from statictext within w_proc_actualiza_proyeccion
end type
end forward

global type w_proc_actualiza_proyeccion from w_para_informes
integer x = 14
integer y = 32
integer width = 2821
integer height = 1196
string title = "ACTUALIZA PROYECCIÓN"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
event ue_validapassword ( )
st_1 st_1
st_2 st_2
end type
global w_proc_actualiza_proyeccion w_proc_actualiza_proyeccion

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_zona, idwc_planta, idwc_cliente, idwc_productor, idwc_especie
end variables

forward prototypes
public function boolean existeproceso (integer cliente, integer planta, date fecha)
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Predios y Cuarteles"
istr_mant.Argumento[2]	=	gstr_parametros.clave

IF isnull(istr_mant.Argumento[2]) THEN
	istr_mant.Argumento[2] = ''
END IF	

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean existeproceso (integer cliente, integer planta, date fecha);Long	ll_cuenta


SELECT COUNT(*)
INTO  :ll_cuenta
FROM dbo.facturprodenca
WHERE clie_codigo=:Cliente
AND   plde_codigo=:Planta
AND   faen_fechaf=:Fecha;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla Facturprodenca")
	RETURN True
ELSEIF ll_cuenta > 0 THEN
       RETURN True		 
ELSE
	    RETURN False
END IF

end function

on w_proc_actualiza_proyeccion.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
end on

on w_proc_actualiza_proyeccion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
end on

event open;call super::open;PostEvent("ue_validapassword")
end event

event resize;call super::resize;st_2.x	=	st_Titulo.x
st_1.x	=	st_2.x	 + 50
end event

type pb_excel from w_para_informes`pb_excel within w_proc_actualiza_proyeccion
end type

type st_computador from w_para_informes`st_computador within w_proc_actualiza_proyeccion
end type

type st_usuario from w_para_informes`st_usuario within w_proc_actualiza_proyeccion
end type

type st_temporada from w_para_informes`st_temporada within w_proc_actualiza_proyeccion
end type

type p_logo from w_para_informes`p_logo within w_proc_actualiza_proyeccion
end type

type st_titulo from w_para_informes`st_titulo within w_proc_actualiza_proyeccion
integer x = 206
integer width = 1897
string text = "Actualiza Proyección "
end type

type pb_acepta from w_para_informes`pb_acepta within w_proc_actualiza_proyeccion
integer x = 2373
integer y = 396
integer taborder = 70
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
end type

event pb_acepta::clicked;Integer li_estado

li_estado = gstr_parametros.estado

st_1.Text = ''

IF li_estado = 1 THEN
	MessageBox("Atención","Proyección Ya fue Ejecutada en esta Temporada")
	Return 1
ELSE	

	DECLARE ActualizaProyecion PROCEDURE FOR dbo.Predcua_actualiza_proyeccion; 
				
	EXECUTE ActualizaProyecion;
	
	IF sqlca.sqlcode < 0 THEN
		F_ErrorBaseDatos(sqlca,"El proceso de Actualización " +&
			  " no se ejecutó Exitosamente")
		Close ActualizaProyecion;
	ELSE
		Close ActualizaProyecion;
	
		st_1.Text = 'Proceso ejecutado Exitosamente'
		Update dbo.parampredioscuarteles set
		papc_estado = 1;
		
		Commit;
		parametros()
	END IF					
END IF
end event

type pb_salir from w_para_informes`pb_salir within w_proc_actualiza_proyeccion
integer x = 2373
integer y = 716
integer taborder = 80
end type

type st_1 from statictext within w_proc_actualiza_proyeccion
integer x = 256
integer y = 640
integer width = 1792
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 553648127
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_proc_actualiza_proyeccion
integer x = 201
integer y = 504
integer width = 1897
integer height = 372
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

