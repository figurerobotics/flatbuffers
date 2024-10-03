

import unittest

import monster_generated

import numpy as np


class MonsterGeneratedTest(unittest.TestCase):

    def test_vec3_simple(self):
        v = monster_generated.Vec3()
        self.assertEqual(v.x, 0.0)
        self.assertEqual(v.y, 0.0)
        self.assertEqual(v.z, 0.0)

        v.x = 1.5
        v.y = 2.5
        v.z = 3.5
        self.assertAlmostEqual(v.x, 1.5)
        self.assertAlmostEqual(v.y, 2.5)
        self.assertAlmostEqual(v.z, 3.5)

        w = monster_generated.Vec3(x=-1.1, y=-2.2, z=-3.3)
        self.assertAlmostEqual(w.x, -1.1)
        self.assertAlmostEqual(w.y, -2.2)
        self.assertAlmostEqual(w.z, -3.3)

    def test_weapon_simple(self):
        weapon = monster_generated.WeaponT()
        self.assertEqual(weapon.name, "")
        self.assertEqual(weapon.damage, 0)

        weapon.name = "sword"
        weapon.damage = 10
        self.assertEqual(weapon.name, "sword")
        self.assertEqual(weapon.damage, 10)

        weapon.damage += 5
        self.assertEqual(weapon.damage, 15)

        bow = monster_generated.WeaponT(name="bow", damage=50)
        self.assertEqual(bow.name, "bow")
        self.assertEqual(bow.damage, 50)

    def test_monster_simple(self):
        m0 = monster_generated.MonsterT()
        self.assertIsNone(m0.pos)
        self.assertEqual(m0.mana, 150)
        self.assertEqual(m0.hp, 100)
        self.assertEqual(m0.name, "")
        self.assertSequenceEqual(m0.inventory, [])
        self.assertEqual(m0.color, monster_generated.Color.Blue)
        self.assertSequenceEqual(m0.weapons, [])
        self.assertIsNone(m0.equipped)
        self.assertSequenceEqual(m0.path, [])

    def test_monster_numpy(self):
        m0 = monster_generated.MonsterT(pos=monster_generated.Vec3(x=1.1, y=2.2, z=3.3),
                                        name="wizard", inventory=[1, 2, 0])
        np.testing.assert_array_equal(m0.inventory, [1, 2, 0])
        m0.inventory.numpy()[2] += 3
        np.testing.assert_array_equal(m0.inventory, [1, 2, 3])
        data = m0.pack(bytearray())

        m1 = monster_generated.Monster.get_root(data)
        buf = np.array(m1.inventory, copy=False)
        self.assertEqual(buf.shape, (3,))
        self.assertEqual(buf.dtype, np.uint8)

        buf[:] += np.array([1, 3, 5], dtype=np.uint8)
        np.testing.assert_array_equal(m1.inventory, [2, 5, 8])

        buf_np = m1.inventory.numpy()
        self.assertEqual(buf_np.shape, (3,))
        self.assertEqual(buf_np.dtype, np.uint8)
        np.testing.assert_array_equal(buf_np, [2, 5, 8])

    def test_monster_pack_unpack(self):
        m0 = monster_generated.MonsterT(pos=monster_generated.Vec3(x=1.1, y=2.2, z=3.3),
                                        name="wizard", inventory=[1, 2, 3])
        data = m0.pack(bytearray())

        m1 = monster_generated.Monster.get_root(data)
        self.assertAlmostEqual(m1.pos.x, 1.1)
        self.assertAlmostEqual(m1.pos.y, 2.2)
        self.assertAlmostEqual(m1.pos.z, 3.3)
        self.assertEqual(m1.name, "wizard")
        self.assertSequenceEqual(m1.inventory, [1, 2, 3])

        m2 = monster_generated.MonsterT()
        m1.unpack_to(m2)
        self.assertEqual(m0.pos.x, m2.pos.x)
        self.assertEqual(m0.pos.y, m2.pos.y)
        self.assertEqual(m0.pos.z, m2.pos.z)
        self.assertEqual(m0.name, m2.name)
        np.testing.assert_array_equal(m0.inventory.numpy(), m2.inventory.numpy())


if __name__ == "__main__":
    unittest.main()
